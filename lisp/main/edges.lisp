(in-package :savage-worlds)

(defclass edge ( )
  ((id :initform (error "Must provide an id")
       :initarg :id
       :reader edge-id)
   (name :initform (error "Must provide a name")
	 :initarg :name
	 :reader name)))

(defclass requirement ()())

(defclass background-edge( edge)())

(defclass combat-edge( edge)())

(defclass leadership-edge( edge)())

(defclass power-edge( edge)())

(defclass professional-edge( edge)())

(defclass social-edge( edge)())

(defclass weird-edge( edge)())

(defclass wild-card-edge( edge)())

(defclass legendary-edge( edge)())

(defvar *edges* '())

(defun background-edge (&key name )
  (let ((edge (make-instance 'background-edge
			     :id (list-length *edges*)
			     :name name)))
    (push edge *edges*)
    edge))

(defvar attractive (background-edge :name 'attractive ))

(defvar very-attractive (background-edge :name 'very-attractive))
