(in-package :cl-user)
(defpackage lime-test
  (:use :cl :fiveam))
(in-package :lime-test)

;;; Utilities

(defparameter *hostname* "0.0.0.0")

(defparameter *impl*
  (if (uiop:getenv "TRAVIS")
      "cl"
      "sbcl"))

(defun read-all-from-stream (stream)
  "Read characters from a stream into a string until EOF."
  (concatenate 'string
               (loop for byte = (read-char-no-hang stream nil nil)
                     while byte collecting byte)))

(defmacro with-swank-lisp ((port) &body body)
  (alexandria:with-gensyms (code process)
    `(let* ((,code (list "(ql:quickload :swank)"
                         "(setf swank:*configure-emacs-indentation* nil)"
                         (format nil
                                 "(let ((swank::*loopback-interface* ~S))
                                    (swank:create-server :port ~D :dont-close t))"
                                 *hostname*
                                 ,port)
                         "(print 'done)"))
            (,process (external-program:start "sbcl" (list "--noinform")
                                              :input :stream
                                              :output :stream)))
       ;; Send input
       (let ((stream (external-program:process-input-stream ,process)))
         (loop for form in ,code do
           (write-string form stream)
           (write-char #\Newline stream)
           (finish-output stream)))
       ;; Wait until done
       (flet ((process-stdout ()
                (read-all-from-stream
                 (external-program:process-output-stream ,process))))
         (let ((output (process-stdout)))
           (loop until (search "DONE" output) do
             (let ((new-output (process-stdout)))
               (setf output (concatenate 'string output new-output))))))
       (when (uiop:getenv "TRAVIS")
         (sleep 5))
       ;; Run the code
       (unwind-protect
            (progn
              ,@body)
         (external-program:signal-process ,process :killed)))))

(defparameter *port* 40000)

(defmacro with-connection ((conn) &body body)
  (let ((port (gensym)))
    `(let ((,port (incf *port*)))
       (with-swank-lisp (,port)
         (let ((,conn (lime:make-connection (uiop:hostname)
                                            ,port
                                            :logp nil)))
           (is-true
            (lime:connect ,conn))
           ,@body)))))

;;; Tests

(def-suite tests
  :description "lime tests.")
(in-suite tests)

(test process
  (with-connection (conn)
    (is
      (stringp (lime:connection-swank-version conn)))
    (is-false
      (lime:debuggerp conn))
    (is-false
      (lime:connection-reader-waiting-p conn))
    (is-false
      (lime:pull-all-events conn))))

(test evaluate
  (with-connection (conn)
    (is
     (stringp (lime:evaluate conn "(+ 2 2)")))
    (sleep 0.1)
    (let ((events (lime:pull-all-events conn)))
      (is
       (equal (length events)
              2))
      (is
       (equal (lime:event-string (first events))
              "4")))))

(test debugger
  (with-connection (conn)
    ;; Trigger an error
    (is-false
      (lime:debuggerp conn))
    (is
     (stringp (lime:evaluate conn "(error \"message\")")))
    ;; Read events
    (sleep 0.1)
    (let ((events (lime:pull-all-events conn)))
      (is
       (equal (length events)
              1))
      (let ((debug (first events)))
        (is
         (typep debug 'lime:debugger-event))))
    ;; Check debug level
    (is-true
      (lime:debuggerp conn))
    (is
     (equal (lime:connection-debug-level conn)
            1))
    ;; Break out
    (is-true
      (lime:abort-debugger conn))
    ;; Pull events
    (sleep 0.1)
    (let ((events (lime:pull-all-events conn)))
      (is-false events))
    ;; Check debug flags
    (is-false
     (lime:debuggerp conn))))

(test standard-input
  (with-connection (conn)
    (is-false
      (lime:connection-reader-waiting-p conn))
    ;; Trigger a read
    (is
     (stringp (lime:evaluate conn "(read)")))
    (sleep 0.1)
    (let ((events (lime:pull-all-events conn)))
      (is
       (typep (first events) 'lime:read-string-event)))
    (is-true
     (lime:connection-reader-waiting-p conn))
    ;; Send some input
    (is
     (stringp (lime:send-input conn "1")))
    ;; Read result
    (sleep 0.1)
    (let ((events (lime:pull-all-events conn)))
      (is
       (equal (length events)
              2))
      (is
       (equal (lime:event-string (first events))
              "1")))))

(test change-package
  (with-connection (conn)
    ;; Create a package
    (is
     (stringp (lime:evaluate conn "(defpackage new.pack (:use :cl))")))
    ;; Dump all events
    (finishes
      (lime:pull-all-events conn))
    ;; Switch package
    (is
     (stringp (lime:evaluate conn "(in-package :new.pack)")))
    (sleep 0.1)
    (let ((events (lime:pull-all-events conn)))
      (is
       (equal (length events)
              5))
      (let ((switch (first (last events))))
        (is
         (typep switch 'lime:switch-package-event))
        (is
         (equal (lime:event-package-name switch)
                "NEW.PACK"))
        (is
         (equal (lime:event-prompt-string switch)
                "PACK"))))))

(run! 'tests)
