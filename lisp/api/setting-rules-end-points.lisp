(in-package :savage-worlds-api)

(defun setting-rules-get-all ()
  (hunchentoot::log-message* :debug "setting-rules-get-all")
  (setf (hunchentoot:content-type*) "application/json") 
  (format nil "{\"setting-rule\":~a}" (encode-json-to-string 
					   savage-worlds::*setting-rules*)))

(defun setting-rules-get-one ()
  (hunchentoot::log-message* :debug "setting-rules-get-one")
  (setf (hunchentoot:content-type*) "application/json") 
  (let* ((id (parse-integer (getf *route-params* :id))))
    (hunchentoot::log-message* :debug "id ~a" id)
    (format nil "{\"setting-rule\":~a}" (encode-json-to-string (first (savage-worlds::find-setting-rules-by-id (list id)))))))
