(in-package :cl-ddd)

(defun users-post()
  (let* ((input-string (hunchentoot::raw-post-data :force-text t))
	 (input-json (rest (first (decode-json-from-string input-string))))
	 (name (string-trim " " (rest (assoc :username input-json))))
	 (password (string-trim " " (rest (assoc :password input-json)))))	 
    (setf (hunchentoot:content-type*) "application/json") 
    (let ((new-user (signup name password)))
      (if (typep new-user 'user)
	  (format nil "{\"user\":~a}" (json:encode-json-to-string new-user))
	  (progn
	    (setf (hunchentoot:return-code*) 422)
	    (format nil "{\"errors\":~a}" new-user))))))

