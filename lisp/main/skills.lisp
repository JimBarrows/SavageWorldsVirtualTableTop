(in-package :savage-worlds)

(defclass skill-description ()
  ((id :reader id :initarg :id :initform (error "Id must be provided"))
   (name :initarg :name
	 :reader name
	 :initform (error "Must provide a name"))
   (attribute :initarg :attribute
	      :reader attribute
	      :initform (error "Must provide an attribute"))))

;The default skill descriptions any setting can have 
(defvar *boating*   (make-instance 'skill-description :id 1 :name 'boating :attribute 'agility))
(defvar *climbing*  (make-instance 'skill-description :id 2 :name 'climbing :attribute 'strength))
(defvar *driving*   (make-instance 'skill-description :id 3 :name 'driving :attribute 'agility))
(defvar *fighting*  (make-instance 'skill-description :id 4 :name 'fighting :attribute 'agility))
(defvar *gambling*  (make-instance 'skill-description :id 5 :name 'gambling :attribute 'smarts))
(defvar *healing*  (make-instance 'skill-description :id 6 :name 'healing :attribute 'smarts))
(defvar *intimidation*  (make-instance 'skill-description :id 7 :name 'intimidation :attribute 'spirit))
(defvar *investigation*  (make-instance 'skill-description :id 8 :name 'investigation :attribute 'smarts))

(defvar *skill-list* '())

(defun load-skill-list()
  (setf *skill-list* '())
  (push *boating* *skill-list*) 
  (push *climbing* *skill-list*)
  (push *driving* *skill-list*)
  (push *fighting* *skill-list*)
  (push *gambling* *skill-list*)
  (push *healing* *skill-list*)
  (push *intimidation* *skill-list*)
  (push *investigation* *skill-list*))

;convience functions to create a skill class for a description
(defun fighting ( &optional (rank *d4*))
  (make-instance 'skill :name (name *fighting*)
		 :rank rank
		 :description *fighting*))



