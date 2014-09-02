(in-package :savage-worlds-api)

(defun hindrances-get-all()
  (setf (hunchentoot:content-type*) "application/json") 
  (format nil "{\"hindrance\":~a}" (encode-json-to-string 
					   savage-worlds::*hindrances*)))

(defun hindrances-get-one()
(setf (hunchentoot:content-type*) "application/json") 
  (let* ((id (parse-integer (getf *route-params* :id))))
    (hunchentoot::log-message* :debug "id ~a" id)
    (format nil "{\"hindrance\":~a}" 
	    (encode-json-to-string 
	     (find-if #'(lambda (hindrance)
			  (= (savage-worlds::hindrance-id hindrance) id))
		      savage-worlds::*hindrances*)))))
