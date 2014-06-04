(in-package :savage-worlds)

(defclass edge( provides-charisma-bonus)
  ((name :initform (error "Must provide a name")
	 :initarg :name
	 :reader name)
   (requirements :initform ()
		:initarg :requirements
		:reader requirements)
   (traits-boosted :initform ()
		   :initarg :traits
		   :reader traits)
   (description :initform ""
		:initarg :description
		:reader description)))

(defclass background-edge( edge)())

(defclass combat-edge( edge)())

(defclass leadership-edge( edge)())

(defclass power-edge( edge)())

(defclass professional-edge( edge)())

(defclass social-edge( edge)())

(defclass weird-edge( edge)())

(defclass wild-card-edge( edge)())

(defclass legendary-edge( edge)())

