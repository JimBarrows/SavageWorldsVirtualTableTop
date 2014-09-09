(in-package :savage-worlds-api)

(defun setting-rules-get-all ()
  (hunchentoot::log-message* :debug "setting-rules-get-all")
  (setf (hunchentoot:content-type*) "application/json") 
  (format nil "{\"setting-rule\":~a}" (encode-json-to-string 
					   savage-worlds::setting-rule-list)))

(defun setting-rules-get-by-id ()
  (hunchentoot::log-message* :debug "setting-rules-get-one")
  (setf (hunchentoot:content-type*) "application/json") 
  (let* ((id (parse-integer (getf *route-params* :id))))
    (format nil "{\"setting-rule\":~a}" (encode-json-to-string (first (savage-worlds::find-setting-rule-by-id (list id)))))))
