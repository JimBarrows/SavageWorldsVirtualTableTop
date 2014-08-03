(in-package :savage-worlds)

(defvar *plot-point-repository* nil)

(defun load-repositories()
  (setf *plot-point-repository* (make-instance 'plot-point-repository))
  (cl-ddd::load-data *plot-point-repository*))
