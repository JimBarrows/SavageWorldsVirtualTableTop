(in-package :savage-worlds)

(defclass skill-description ()
  ((name :initarg :name
	:reader name
	:initform (error "Must provide a name"))
  (attribute :initarg :attribute
             :initform (error "Must provide an attribute"))))

;The default skill descriptions any setting can have 
(defvar *fighting*   (make-instance 'skill-description :name 'fighting :attribute 'strength))

;convience functions to create a skill class for a description
(defun fighting ( &optional (rank *d4*))
  (make-instance 'skill :name (name *fighting*)
		 :rank rank
		 :description *fighting*))



