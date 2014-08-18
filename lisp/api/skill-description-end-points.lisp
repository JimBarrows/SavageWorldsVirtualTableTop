(in-package :savage-worlds-api)

(defun skill-descriptions-get ()
  (setf (hunchentoot:content-type*) "application/json") 
  (format nil "{\"skill-description\":~a}" (encode-json-to-string savage-worlds::*skill-description-list*)))
