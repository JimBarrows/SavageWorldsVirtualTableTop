(in-package :savage-worlds-api)

(defun setting-rules-get ()
  (hunchentoot::log-message* :debug "setting-rules-get")
  (setf (hunchentoot:content-type*) "application/json") 
  (format nil "{\"setting-rule\":~a}" (encode-json-to-string 
					   savage-worlds::*setting-rules*)))
