(in-package :savage-worlds)

(defclass character-record (base-record)
  ((name
    :accessor name)
   (race :initform *human*
         :initarg :race
         :reader race)
   (cash :initform 500
	 :accessor cash)
   (edges :initform()
	  :accessor edges)
   (hindrances :initform()
	       :accessor hindrances)
   (background :initform "")))

(defmethod add-edge( edge-to-add (record character-record))
  (push edge-to-add (edges record)))

(defmethod add-hindrance( hindrance-to-add (record character-record))
  (push hindrance-to-add (hindrances record)))

(defmethod charisma((record character-record))
   (reduce #'+ (map 'list 
		    #'(lambda (edge)
			(charisma edge))
		    (append (edges record)
			    (hindrances record)))))
