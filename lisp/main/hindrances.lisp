(in-package :savage-worlds)

(defclass hindrance () 
  ((id :initform (error "Must provide an id")
       :initarg :id
       :reader hindrance-id)
   (name :initform (error "Must provide an error for a hindrance")
	 :initarg :name
	 :reader name)))

(defclass major-hindrance( hindrance)())

(defclass minor-hindrance( hindrance)())

(defvar *hindrances* '())

(defun major-hindrance ( &key name)
  (let ((hindrance (make-instance 'major-hindrance :id (list-length *hindrances*) :name name)))
    (push hindrance *hindrances*)
    hindrance))

(defun minor-hindrance ( &key name)
  (let ((hindrance (make-instance 'minor-hindrance :id (list-length *hindrances*) :name name)))
    (push hindrance *hindrances*)
    hindrance))

(defvar all-thumbs (minor-hindrance :name 'all-thumbs))
(defvar anemic (minor-hindrance :name 'anemic))
(defvar arrogant (major-hindrance :name 'arrogant)) 
(defvar bad-eyes (minor-hindrance :name 'bad-eyes))
(defvar very-bad-eyes (major-hindrance :name 'very-bad-eyes))
		
(defun find-hindrances-by-id-list (id-list)
  (remove nil
	  (map 'list (lambda (hindrance)
		       (when (member (hindrance-id hindrance) id-list)
			 hindrance))
	       *hindrances*)))
