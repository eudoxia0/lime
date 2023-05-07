(in-package :cl-user)
(defpackage lime-example
  (:use :cl)
  (:export :repl)
  (:documentation "An example REPL built with Lime."))
(in-package :lime-example)

(defparameter *port* 15001)

(defun start-swank ()
  (setf swank:*configure-emacs-indentation* nil)
  (let ((swank::*loopback-interface* (uiop:hostname)))
    (swank:create-server :port *port* :dont-close t)))

(defmacro with-swank-connection ((conn port) &body body)
  "Start a Swank server and connect to it."
  `(progn
     (format t "Starting Swank server...~%")
     (start-swank)
     (sleep 1)
     (let ((,conn (lime:make-connection (uiop:hostname)
                                        ,port)))
       (format t "Connecting...~%")
       (lime:connect ,conn)
       (format t "Swank server running on ~A ~A~%"
               (lime:connection-implementation-name ,conn)
               (lime:connection-implementation-version ,conn))
       (unwind-protect
            (progn
              ,@body)
         (swank:stop-server ,port)))))

(defun ping-response (conn event)
  (swank-protocol:send-message-string conn
  				      (format nil
				      	      "(:emacs-pong ~A ~A)"
					      (lime:event-thread event)
					      (lime:event-tag event))))

(defun repl ()
  "Start the REPL."
  (with-swank-connection (conn *port*)
    (loop
      (tagbody t
        ;; Read all events
        (sleep 0.05)
        (let ((events (lime:pull-all-events conn)))
          (loop for event in events do
            (typecase event
              (lime:write-string-event
               (write-string (lime:event-string event)))
              (lime:debugger-event
               (write-string "Entered debugger!"))
	      (lime:ping-event
	       (ping-response conn event)
	       ;; loop back around to catch any events after the ping response.
	       (go t))
              (t
               t)))))
      ;; Take input
      (if (lime:connection-reader-waiting-p conn)
          ;; Read a line to send as standard input
          (progn
            (format t "~%Read: ")
            (let ((input (read-line)))
              (lime:send-input conn input)))
          ;; Read regular code
          (progn
            (format t "~A> " (lime:connection-package conn))
            (let ((input (read)))
              (lime:evaluate conn
                             (with-standard-io-syntax
                                 (prin1-to-string input)))))))))
