(in-package :savage-worlds)

(defclass trait-rank ()
  ((rank
    :initarg :rank
    :initform :d4
    :reader rank)
   (modifier 
    :initarg :modifier
    :initform 0
    :reader modifier)
   (value
    :initarg :value
    :initform 4
    :reader value)))

(defvar *d4* (make-instance 'trait-rank))
(defvar *d6* (make-instance 'trait-rank :rank :d6 :value 6))
(defvar *d8* (make-instance 'trait-rank :rank :d8 :value 8))
(defvar *d10* (make-instance 'trait-rank :rank :d10 :value 10))
(defvar *d12* (make-instance 'trait-rank :rank :d12 :value 12))

(defclass trait () 
  ((name
    :initarg :name
    :reader name)
   (rank
    :initarg :rank
    :initform *d4*
    :reader rank)))

(deftype attributes () 
  '(member agility smarts spirit strength vigor))

(defclass attribute( trait)
  ())

(defclass skill( trait)
  ((description :initarg :description)))

(defun agility ( &optional (rank *d4*))
  (make-instance 'attribute :name 'agility
		 :rank rank))

(defun smarts ( &optional (rank *d4*))
  (make-instance 'attribute :name 'smarts
		 :rank rank))

(defun spirit ( &optional (rank *d4*))
  (make-instance 'attribute :name 'spirit
		 :rank rank))

(defun strength ( &optional (rank *d4*))
  (make-instance 'attribute :name 'strength
		 :rank rank))

(defun vigor ( &optional (rank *d4*))
  (make-instance 'attribute :name 'vigor
		 :rank rank))

