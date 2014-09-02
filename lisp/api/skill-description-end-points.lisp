(in-package :savage-worlds-api)

(defun skill-descriptions-get-all ()
  (setf (hunchentoot:content-type*) "application/json") 
  (format nil "{\"skill-description\":~a}" (encode-json-to-string savage-worlds::*skill-description-list*)))

(defun skill-descriptions-get-by-id()
  (setf (hunchentoot:content-type*) "application/json") 
  (let* ((id (parse-integer (getf *route-params* :id))))
    (hunchentoot::log-message* :debug "id ~a" id)
    (format nil "{\"skill-description\":~a}" 
	    (encode-json-to-string 
	     (find-if #'(lambda (skill-description)
			  (= (savage-worlds::id skill-description) id))
		      savage-worlds::*skill-description-list*)))))
