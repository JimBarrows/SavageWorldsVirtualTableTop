(in-package :cl-ddd)
(defvar *user-repository* nil)

(in-package :savage-worlds-api)

(defvar *ht-server* nil)

(defun start-application()
  "Start up the application"
  (setf cl-ddd::*user-repository* (make-instance 'cl-ddd::user-repository))
  (cl-ddd::load-data cl-ddd::*user-repository*)

  (savage-worlds::load-repositories)

  (setf *ht-server* 
	(start (make-instance 'acceptor 
			      :port 8080))))

(defun stop-application()
  "Stop the application cleanly"
  (savage-worlds::unload-repositories)
  (cl-ddd::save-data cl-ddd::*user-repository*)
  (stop *ht-server*)
  (setf *ht-server* nil))

#+sbcl
(progn
  (format t "Starting Savage Worlds")
  (defparameter *running* t)
  (defun main (argv)
    (declare (ignore argv))
    (format t "setting up daemon")
    (sb-daemon:daemonize :output "/tmp/savage-worlds.output"
                         :error "/tmp/savage-worlds.error"
                         :exit-parent t
                         :sigterm (lambda (sig)
                                    (declare (ignore sig))
				    (stop-application)
                                    (setf *running* nil)))
    (setf *running* t)
    (format t "Starting server")
    (start-application)
    (loop while *running* do (sleep 1))))
