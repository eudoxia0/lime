(in-package :cl-user)
(defpackage lime
  (:use :cl)
  (:import-from :trivial-types
                :association-list
                :proper-list)
  ;; Classes
  (:export :connection
           :event
           :write-string-event
           :switch-package-event
           :debugger)
  ;; Accessors
  (:export :connection-debug-level
           :event-string
           :event-package-name
           :event-short-name
           :event-restarts
           :event-call-stack)
  ;; Functions
  (:export :pull-all-events)
  (:documentation "A high-level Swank client."))
(in-package :lime)

(defun parse-response (response)
  "Parse a response from read-event into a more manageable format."
  (list :status (first (second response))
        :value (second (second response))
        :request-id (first (last response))))

;;; Classes

(defclass connection (swank-protocol:connection)
  ((debug-level :accessor connection-debug-level
                :initform 0
                :type integer
                :documentation "The depth at which the debugger is called."))
  (:documentation "A connectio to a Swank server."))

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
               :documentation "An association list of a stack frame's position to its description."))
  (:documentation "Signals that the debugger has been entered."))

;;; Functions and methods

(defun debuggerp (connection)
  "T if the session is in the debugger, NIL otherwise."
  (> (connection-debug-level connection) 0))

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

(defun pull-all-events (connection)
  "Return a list of all events from the connection."
  (let ((messages (swank-protocol:read-all-messages connection)))
    (remove-if #'null
               (loop for message in messages collecting
                 (parse-event connection message)))))
