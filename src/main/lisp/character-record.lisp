(in-package :savage-worlds)

(defclass character-record ()
  ((name
    :accessor name)
   (race :initform *human*
         :initarg :race
         :reader race)
   (agility :initform (create-agility)
            :reader agility
            :initarg :agility)
   (smarts :initform (create-smarts)
           :reader smarts
           :initarg smarts)
   (spirit :initform (create-spirit)
           :reader spirit
           :initarg spirit)
   (strength :initform (create-strength)
             :reader strength
             :initarg strength)
   (vigor :initform (create-vigor)
          :reader vigor
          :initarg vigor)
   (pace :initform 6)
   (skills :initarg :skills
           :initform ()
	   :accessor skills)
   (gear :initform()
	 :accessor gear)
   (cash :initform 500
	 :accessor cash)
   (edges :initform()
	  :accessor edges)
   (hindrances :initform()
	       :accessor hindrances)
   (background :initform "")))

(defmethod add-gear( gear-to-add (record character-record))
  (let ((new-gear (make-instance 'gear :description gear-to-add)))
    (push new-gear (gear record))))

(defmethod add-edge( edge-to-add (record character-record))
  (push edge-to-add (edges record)))

(defmethod add-hindrance( hindrance-to-add (record character-record))
  (push hindrance-to-add (hindrances record)))

(defmethod fighting-skill ((record character-record))
  (find *fighting* (skills record) 
	       :test #'(lambda (l r) (eq (name l) (name r)))))

(defmethod fighting-value ((record character-record))
  (let ((fight-skill (fighting-skill record)))
    (if fight-skill 
	(value (rank fight-skill))
	0)))

(defmethod fighting-modifier ((record character-record))
  (let ((fight-skill (fighting-skill record)))
    (if fight-skill 
	(modifier (rank fight-skill))
	0)))


(defmethod shield-bonus ((record character-record))
  (reduce #'+ (map 'list #'(lambda (gear)
			     (if (eq (find-class 'shield) (class-of (description gear)))
				 (parry (description gear))
				 0))
		   (gear record))))


(defmethod armor-bonus ((record character-record))
  (reduce #'+ (map 'list #'(lambda (gear)
			     (if (eq (find-class 'armor) (class-of (description gear)))
				 (armor (description gear))
				 0))
		   (gear record))))

(defmethod parry ((record character-record))
  (+ 2 
     (half-of (fighting-value record)) 
     (half-of (fighting-modifier record))  
     (shield-bonus record)))

(defmethod toughness ((record character-record))
  (+ 2 
     (half-of (value (rank (vigor record))))
     (half-of ( modifier (rank (vigor record))))
     (armor-bonus record)))



(defmethod charisma((record character-record))
  (+
   (reduce #'+ (map 'list 
		    #'(lambda (edge)
			(charisma edge))
		    (edges record)))
   (reduce #'+ (map 'list 
		    #'(lambda (hindrance)
			(charisma hindrance))
		    (hindrances record)))))
