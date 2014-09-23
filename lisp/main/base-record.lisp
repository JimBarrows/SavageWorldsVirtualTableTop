(in-package :savage-worlds)

(defclass base-record ()
  ((agility :initform (create-agility)
            :reader agility
            :initarg :agility)
   (smarts :initform (create-smarts)
           :reader smarts
           :initarg :smarts)
   (spirit :initform (create-spirit)
           :reader spirit
           :initarg :spirit)
   (strength :initform (create-strength)
             :reader strength
             :initarg :strength)
   (vigor :initform (create-vigor)
          :reader vigor
          :initarg :vigor)
   (pace :initform 6
	 :initarg :pace)
   (skills :initarg :skills
           :initform ()
	   :accessor :skills)
   (gear :initform()
	 :initarg :gear
	 :accessor :gear)))
   
(defmethod add-gear( gear-to-add (record base-record))
  (let ((new-gear (make-instance 'gear :description gear-to-add)))
    (push new-gear (gear record))))

(defmethod fighting-skill ((record base-record))
  (find *fighting* (skills record) 
	       :test #'(lambda (l r) (eq (name l) (name r)))))

(defmethod fighting-value ((record base-record))
  (let ((fight-skill (fighting-skill record)))
    (if fight-skill 
	(value (rank fight-skill))
	0)))

(defmethod fighting-modifier ((record base-record))
  (let ((fight-skill (fighting-skill record)))
    (if fight-skill 
	(modifier (rank fight-skill))
	0)))

(defmethod armor-bonus ((record base-record))
  (reduce #'+ (map 'list #'(lambda (gear)
			     (if (eq (find-class 'armor) (class-of (description gear)))
				 (armor (description gear))
				 0))
		   (gear record))))

(defmethod shield-bonus ((record base-record))
  (reduce #'+ (map 'list #'(lambda (gear)
			     (if (eq (find-class 'shield) (class-of (description gear)))
				 (parry (description gear))
				 0))
		   (gear record))))

(defmethod parry ((record base-record))
  (+ 2 
     (half-of (fighting-value record)) 
     (half-of (fighting-modifier record))  
     (shield-bonus record)))

(defmethod toughness ((record base-record))
  (+ 2 
     (half-of (value (rank (vigor record))))
     (half-of ( modifier (rank (vigor record))))
     (armor-bonus record)))

