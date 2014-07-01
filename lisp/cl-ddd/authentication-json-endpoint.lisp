(in-package :cl-ddd)

(defun users-post()
  (let* ((input-string (hunchentoot::raw-post-data :force-text t))
	 (input-json (rest (first (decode-json-from-string input-string))))
	 (name (string-trim " " (rest (assoc :username input-json))))
	 (password (string-trim " " (rest (assoc :password input-json)))))	 
    (setf (content-type*) "application/json") 
    (let ((add-user-result (add-user name password)))
      (if (typep add-user-result 'user)
	  (format nil "{\"user\":~a}" (json:encode-json-to-string new-user))
	  (progn
	    (setf (return-code*) 422)
	    (format nil "{\"errors\":~a}" add-user-result))))))

(defun charge() 
  (let* ((stripe-token (hunchentoot::parameter "stripeToken"))
	 (email (hunchentoot:parameter "stripeEmail"))
	 (customer (stripe:sstruct-get (stripe::create-customer 
					:card stripe-token 
					:description email) :id)))
    (stripe::create-charge :amount 1000 :currency "usd" :customer customer )
    (format nil "###################################### Hey~@[ ~A~]!" stripe-token)))
