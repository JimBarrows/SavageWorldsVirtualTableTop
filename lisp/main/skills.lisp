(in-package :savage-worlds)

(defclass skill-description ()
  ((id :reader id :initarg :id :initform (error "Id must be provided"))
   (name :initarg :name
	 :reader name
	 :initform (error "Must provide a name"))
   (attribute :initarg :attribute
	      :reader attribute
	      :initform (error "Must provide an attribute"))))

(defun skill-description (&key name attribute)
  (let ((skill-description (make-instance 'skill-description :id (list-length *skill-descriptions*) :name name :attribute attribute)))
    (push skill-description *skill-descriptions*)
    skill-description))

(defun find-skill-description-by-id (skill-description-ids)
  (remove nil 
	  (map 'list (lambda (skill-description) 
		       (when (member (id skill-description) skill-description-ids) 
			 skill-description)) 
	       *skill-descriptions*)))

