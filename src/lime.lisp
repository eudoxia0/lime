(in-package :cl-user)
(defpackage lime
  (:use :cl)
  (:import-from :trivial-types
                :association-list
                :proper-list)
  (:import-from :swank-protocol
                :connect)
  ;; Classes
  (:export :connection
           :event
           :write-string-event
           :switch-package-event
           :debugger-event)
  ;; Accessors
  (:export :connection-debug-level
           :event-string
           :event-package-name
           :event-short-name
           :event-restarts
           :event-call-stack)
  ;; Functions and methods
  (:export :make-connection
           :connect
           :pull-all-events
           :debuggerp)
  (:documentation "A high-level Swank client."))
(in-package :lime)

;;; Classes

(defclass connection (swank-protocol:connection)
  ((debug-level :accessor connection-debug-level
                :initform 0
                :type integer
                :documentation "The depth at which the debugger is called.")
   (server-pid :accessor connection-server-pid
               :type integer
               :documentation "The PID of the Swank server process.")
   (server-impl-name :accessor connection-server-impl-name
                     :type string
                     :documentation "The name of the implementation running the
 Swank server.")
   (server-impl-version :accessor connection-server-impl-version
                        :type string
                        :documentation "The version string of the implementation
 running the Swank server.")
   (server-machine-type :accessor connection-server-machine-type
                        :type string
                        :documentation "The server machine's architecture.")
   (server-machine-version :accessor connection-server-machine-version
                           :type string
                           :documentation "The server machine's processor type.")
   (server-swank-version :accessor connection-server-swank-version
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

(defclass debugger (event)
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

;;; Parsing messages into events

(defun parse-event (connection message)
  "Parse a swank-protocol message S-expression into a an instance of a subclass
of event, or return NIL."
  (alexandria:destructuring-case message
    ((:write-string string)
     ;; Write a string to the REPL's output
     (make-instance 'write-string-event
                    :string string))
    ((:new-package name prompt-string)
     ;; Change the REPL package
     (make-instance 'switch-package-event
                    :package-name name
                    :prompt-string prompt-string))
    ((:debug thread level condition restarts frames continuations)
     ;; Parse debugging information
     (declare (ignore level thread continuations))
     (make-instance 'debug
                    :condition (remove-if #'null condition)
                    :restarts (loop for restart in restarts collecting
                                (cons (first restart) (second restart)))
                    :call-stack (loop for frame in frames collecting
                                  (cons (first frame) (second frame)))))
    ((:debug-activate x y z)
     ;; Entered the debugger
     (declare (ignore x y z))
     (incf (connection-debug-level connection))
     nil)
    ((:debug-return x y z)
     ;; Left the debugger
     (declare (ignore x y z))
     (decf (connection-debug-level connection))
     nil)
    ((t &rest rest)
     (declare (ignore rest))
     nil)))

;;; Functions and methods

(defun make-connection (hostname port)
  "Create a connection object."
  (make-instance 'connection
                 :hostname hostname
                 :port port))

(defmethod connect :after ((connection connection))
  "After connecting, query the Swank server for connection information and
create a REPL."
  ;; Issue every request
  (swank-protocol:request-connection-info connection)
  (swank-protocol:request-swank-require connection
                                        '(slime-presentations slime-repl))
  (swank-protocol:request-init-presentations connection)
  (swank-protocol:request-create-repl connection)
  ;; Read the connection information message
  (let* ((info (swank-protocol:read-message connection))
         (data (getf (getf info :return) :ok))
         (impl (getf data :implementation))
         (machine (getf data :machine))
         (machine-type (getf machine :type)))
    (setf (connection-server-pid connection)
          (getf data :pid)

          (connection-server-impl-name connection)
          (getf impl :name)

          (connection-server-impl-version connection)
          (getf impl :version)

          (connection-server-machine-type connection)
          (getf machine :type)

          (connection-server-machine-version connection)
          (getf machine :version)

          (connection-server-version connection)
          (getf data :version)))
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
