(in-package :savage-worlds)

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

(defun default-agility ( &optional (rank *d4*))
  (make-instance 'attribute :name 'agility
		 :rank rank))

(defun default-smarts ( &optional (rank *d4*))
  (make-instance 'attribute :name 'smarts
		 :rank rank))

(defun default-spirit ( &optional (rank *d4*))
  (make-instance 'attribute :name 'spirit
		 :rank rank))

(defun default-strength ( &optional (rank *d4*))
  (make-instance 'attribute :name 'strength
		 :rank rank))

(defun default-vigor ( &optional (rank *d4*))
  (make-instance 'attribute :name 'vigor
		 :rank rank))

