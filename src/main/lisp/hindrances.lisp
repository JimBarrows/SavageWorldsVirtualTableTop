(in-package :savage-worlds)

(defclass hindrance () 
  ((name :initform (error "Must provide an error for a hindrance")
	 :initarg :name
	 :reader name)
   (traits-boosted :initform ()
		   :initarg :traits
		   :reader traits)
   (description :initform ""
		:initarg :description
		:reader description)
   (charisma :initform 0
	     :initarg :charisma
	     :reader charisma)))

(defclass major-hindrance( hindrance)())

(defclass minor-hindrance( hindrance)())

(defvar bloodthirsty ( make-instance 'major-hindrance :name 'bloodthirsty
				     :charisma -4))
