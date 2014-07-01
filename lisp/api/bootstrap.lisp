(in-package :savage-worlds-api)

(defvar *ht-server* nil)
(defvar *task-repository* nil)
(defvar *user-repository* nil)

(defun start-application()
  "Start up the application"
  (setf *user-repository* (make-instance 'user-repository))
  (load-data *user-repository*)
  (setf *ht-server* 
	(start (make-instance 'acceptor 
			      :port 8080))))

(defun stop-application()
  "Stop the application cleanly"
  (save-data *user-repository*)
  (stop *ht-server*)
  (setf *ht-server* nil))
