(in-package :savage-worlds)

(defvar *setting-repository* nil)

(defun load-repositories()
  (setf *setting-repository* (make-instance 'setting-repository))
  (cl-ddd::load-data *setting-repository*))
