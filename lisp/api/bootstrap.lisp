(in-package :cl-ddd)
(defvar *user-repository* nil)

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
  (cl-ddd::save-data *user-repository*)
  (stop *ht-server*)
  (setf *ht-server* nil))
