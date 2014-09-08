(in-package :savage-worlds-api)

(defun edges-get-all()
  (setf (hunchentoot:content-type*) "application/json") 
  (format nil "{\"edges\":~a}" (encode-json-to-string 
					   savage-worlds::edge-list)))

(defun edges-get-by-id()
(setf (hunchentoot:content-type*) "application/json") 
  (let* ((id (parse-integer (getf *route-params* :id))))
    (hunchentoot::log-message* :debug "id ~a" id)
    (format nil "{\"edges\":~a}" 
	    (encode-json-to-string 
	     (find-if #'(lambda (edge)
			  (= (savage-worlds::edge-id edge) id))
		      savage-worlds::edge-list)))))
