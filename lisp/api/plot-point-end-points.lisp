(in-package :savage-worlds-api)

(defun plot-points-get ()
 (setf (hunchentoot:content-type*) "application/json") 
 (format nil "{\"plot-point\":~a}" (json:encode-json-to-string 
				   (cl-ddd::list-data savage-worlds::*plot-point-repository*))))
