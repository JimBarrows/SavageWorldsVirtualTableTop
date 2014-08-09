(in-package :cl-ddd)

(defun signup( name password)
  (when (string= "" name)
    (return-from signup "{ \"username\" : [\"Username cannot be blank\"]}"))
  (when (string= "" password)
    (return-from signup "{ \"password\" : [\"Password cannot be blank\"]}"))
  (when (username-exists-p *user-repository* name)
    (return-from signup "{ \"username\" : [\"Username exists\"]}"))
  (let ((new-user (make-instance 'user :username name :password password)))
    (add *user-repository* new-user)
    (return-from signup new-user)))

(defun login( name password)
  (let ((user (find-by-username *user-repository* name)))
    (cond
      ((or (string= "" name)
	   (string= "" password))
       nil)
      ((and (username-exists-p *user-repository* name)
	    (string= name (username user))
	    (string= password (password user)))
       user))))
      
    

