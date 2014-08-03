(in-package :savage-worlds-api)

(defun plot-points-get ()
 (setf (hunchentoot:content-type*) "application/json") 
 (let ((plot-point-list (cl-ddd::list-data savage-worlds::*plot-point-repository*)))
   (if plot-point-list
       (format nil "{\"plot-point\":~a}" (json:encode-json-to-string 
				      (cl-ddd::list-data plot-point-list)))
       (format nil "{\"plot-point\":[]}"))))
