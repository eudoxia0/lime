(in-package :cl-user)
(defpackage lime
  (:use :cl)
  (:import-from :trivial-types
                :association-list
                :proper-list)
  (:import-from :swank-protocol
                :connect
                :connection-package)
  ;; Classes
  (:export :connection
           :event
           :write-string-event
           :switch-package-event
           :debugger-event
           :read-string-event
	   :ping-event)
  ;; Accessors
  (:export :connection-package
           :connection-debug-level
           :connection-reader-waiting-p
           :connection-pid
           :connection-implementation-name
           :connection-implementation-version
           :connection-machine-type
           :connection-machine-version
           :connection-swank-version
           :event-string
           :event-package-name
           :event-prompt-string
           :event-condition
           :event-restarts
           :event-call-stack
	   :event-thread
	   :event-tag)
  ;; Functions and methods
  (:export :make-connection
           :connect
           :pull-all-events
           :debuggerp
           :evaluate
           :send-input
           :abort-debugger)
  (:documentation "A high-level Swank client."))
(in-package :lime)

;;; Classes

(defclass connection (swank-protocol:connection)
  ((debug-level :accessor connection-debug-level
                :initform 0
                :type integer
                :documentation "The depth at which the debugger is called.")
   (reader-waiting :accessor connection-reader-waiting-p
                   :initform nil
                   :type boolean
                   :documentation "Whether or not the server is waiting for
 input on standard input.")
   (pid :accessor connection-pid
        :type integer
        :documentation "The PID of the Swank server process.")
   (implementation-name :accessor connection-implementation-name
                        :type string
                        :documentation "The name of the implementation running
 the Swank server.")
   (implementation-version :accessor connection-implementation-version
                           :type string
                           :documentation "The version string of the
 implementation running the Swank server.")
   (machine-type :accessor connection-machine-type
                 :type string
                 :documentation "The server machine's architecture.")
   (machine-version :accessor connection-machine-version
                    :type string
                    :documentation "The server machine's processor type.")
   (swank-version :accessor connection-swank-version
                  :type string
                  :documentation "The server's Swank version."))
  (:documentation "A connection to a Swank server."))

(defclass event ()
  ()
  (:documentation "The base class of all events"))

(defclass write-string-event (event)
  ((string :reader event-string
           :initarg :string
           :type string
           :documentation "The string to write."))
  (:documentation "A write-string event."))

(defclass switch-package-event (event)
  ((package-name :reader event-package-name
                 :initarg :package-name
                 :type string
                 :documentation "The full name of the new package.")
   (prompt-string :reader event-prompt-string
                  :initarg :prompt-string
                  :type string
                  :documentation "The name that should be displayed in the
 REPL. If the package has periods in the name, this is usually the subsequence
 after the last period, e.g: 'app.db.user' => 'user'."))
  (:documentation "An event that indicates the inferior Lisp has changed its
  current package."))

(defclass debugger-event (event)
  ((condition :reader event-condition
              :initarg :condition
              :type (proper-list string)
              :documentation "A list of strings describing the condition.")
   (restarts :reader event-restarts
             :initarg :restarts
             :type (association-list string string)
             :documentation "An association list of restart names to descriptions.")
   (call-stack :reader event-call-stack
               :initarg :call-stack
               :type (association-list integer string)
               :documentation "An association list of a stack frame's position
 to its description."))
  (:documentation "Signals that the debugger has been entered."))

(defclass ping-event ()
   ((thread :reader event-thread
	    :initarg :thread
	    :type string
	    ;; TODO: finish documentation.
	    :documentation "")
    (tag :reader event-tag
	 :initarg :tag
	 :type string
	 ;; TODO: finish documentation.
	 :documentation ""))
   (:documentation "An event that checks if the client is still alive.
   		   When this event is recieved, Swank waits until it recieves (:PONG thread tag)"))

(defclass read-string-event (event)
  ()
  (:documentation "Signals that the server is waiting for input."))

;;; Printing events

(defmethod print-object ((event write-string-event) stream)
  "Print a write-string event."
  (print-unreadable-object (event stream :type t)
    (format stream "~S" (event-string event))))

(defmethod print-object ((event switch-package-event) stream)
  "Print a switch-package event."
  (print-unreadable-object (event stream :type t)
    (format stream "~S (~S)"
            (event-package-name event)
            (event-prompt-string event))))

(defmethod print-object ((event debugger-event) stream)
  "Print a debugger event."
  (print-unreadable-object (event stream :type t)
    (format stream "~{~A ~}, ~D restarts, ~D stack frames"
            (event-condition event)
            (length (event-restarts event))
            (length (event-call-stack event)))))

;;; Parsing messages into events

(defun parse-event (connection message)
  "Parse a swank-protocol message S-expression into a an instance of a subclass
of event, or return NIL."
  (alexandria:destructuring-case message
    ((:write-string string &rest rest)
     (declare (ignore rest))
     ;; Write a string to the REPL's output
     (make-instance 'write-string-event
                    :string string))
    ((:new-package name prompt-string)
     ;; Change the REPL package
     (setf (swank-protocol:connection-package connection)
           name)
     (make-instance 'switch-package-event
                    :package-name name
                    :prompt-string prompt-string))
    ;; Debugger
    ((:debug thread level condition restarts frames continuations)
     ;; Parse debugging information
     (declare (ignore level thread continuations))
     (make-instance 'debugger-event
                    :condition (remove-if #'null condition)
                    :restarts (loop for restart in restarts collecting
                                (cons (first restart) (second restart)))
                    :call-stack (loop for frame in frames collecting
                                  (cons (first frame) (second frame)))))
    ((:debug-activate &rest rest)
     ;; Entered the debugger
     (declare (ignore rest))
     (incf (connection-debug-level connection))
     nil)
    ((:debug-return &rest rest)
     ;; Left the debugger
     (declare (ignore rest))
     (decf (connection-debug-level connection))
     nil)
    ;; Standard input reading
    ((:read-string &rest rest)
     (declare (ignore rest))
     (setf (connection-reader-waiting-p connection) t)
     (make-instance 'read-string-event))
    ;; Ping
    ((:ping thread tag &rest rest)
     (declare (ignore rest))
     (make-instance 'ping-event :thread thread :tag tag))
    ;; Else
    ((t &rest rest)
     (declare (ignore rest))
     nil)))

;;; Functions and methods

(defun make-connection (hostname port &key logp)
  "Create a connection object."
  (make-instance 'connection
                 :hostname hostname
                 :port port
                 :logp logp))

(defmethod connect :after ((connection connection))
  "After connecting, query the Swank server for connection information and
create a REPL."
  ;; Issue every request
  (swank-protocol:request-connection-info connection)
  ;; Read the connection information message
  (let* ((info (swank-protocol:read-message connection))
         (data (getf (getf info :return) :ok))
         (impl (getf data :lisp-implementation))
         (machine (getf data :machine)))
    (setf (connection-pid connection)
          (getf data :pid)

          (connection-implementation-name connection)
          (getf impl :name)

          (connection-implementation-version connection)
          (getf impl :version)

          (connection-machine-type connection)
          (getf machine :type)

          (connection-machine-version connection)
          (getf machine :version)

          (connection-swank-version connection)
          (getf data :version)))
  ;; Require some Swank modules
  (swank-protocol:request-swank-require connection
                                        '(swank-presentations swank-repl))
  (swank-protocol:read-message connection)
  ;; Start it up
  (swank-protocol:request-init-presentations connection)
  (swank-protocol:request-create-repl connection)
  ;; Wait for startup
  (swank-protocol:read-message connection)
  ;; Read all the other messages, dumping them
  (swank-protocol:read-all-messages connection))

(defun pull-all-events (connection)
  "Return a list of all events from the connection."
  (let ((messages (swank-protocol:read-all-messages connection)))
    (remove-if #'null
               (loop for message in messages collecting
                 (parse-event connection message)))))

(defun debuggerp (connection)
  "T if the connection is in the debugger, NIL otherwise."
  (> (connection-debug-level connection) 0))

(defun evaluate (connection string)
  "Send a string to the Swank server for evaluation."
  (swank-protocol:request-listener-eval connection string)
  string)

(defun send-input (connection string)
  "Send a string to the Swank server's standard input."
  (when (connection-reader-waiting-p connection)
    (swank-protocol:request-input-string-newline connection string)
    (setf (connection-reader-waiting-p connection) nil)
    string))

(defun abort-debugger (connection)
  "Leave the debugger."
  (when (debuggerp connection)
    (swank-protocol:request-throw-to-toplevel connection)
    t))
