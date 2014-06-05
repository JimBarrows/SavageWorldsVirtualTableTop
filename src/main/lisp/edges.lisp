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
		:reader description)
   (charisma :initform 0
	     :initarg :charisma
	     :reader charisma)))

(defclass requirement ()())

(defclass min-rank ( requirement) 
  ((rank :initform 'novice
	:initarg :is
	:reader is)))

(defclass min-skill ( requirement)
  ((skill :initform (error "Must provide a skill")
	 :initarg :is
	 :reader is)))

(defclass min-trait ( requirement)
  ((trait :initform (error "Must provide a trait")
	 :initarg :is
	 :reader is)))

(defclass edge-required (requirement)
  ((edge :initform (error "Must provide the edge that is required")
	:initarg :is
	:reader is)))

(defclass background-edge( edge)())

(defvar attractive (make-instance 'background-edge 
				  :name 'attractive 
				  :requirements '((min-rank :is 'novice) (min-skill :is (vigor *d6*)))
				  :charisma 2))

(defvar very-attractive (make-instance 'background-edge
				       :name 'very-attractive
				       :requirements '((min-rank :is 'novice)
						       (edge-required :is attractive))
				       :charisma 4))

(defclass combat-edge( edge)())

(defclass leadership-edge( edge)())

(defclass power-edge( edge)())

(defclass professional-edge( edge)())

(defclass social-edge( edge)())

(defclass weird-edge( edge)())

(defclass wild-card-edge( edge)())

(defclass legendary-edge( edge)())

