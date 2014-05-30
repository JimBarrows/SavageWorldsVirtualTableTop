(in-package :savage-worlds)

(defclass character-record ()
  ((name
    :accessor name)
   (race :initform *human*
         :initarg :race
         :reader race)
   (agility :initform (default-agility)
            :reader agility
            :initarg :agility)
   (smarts :initform (default-smarts)
           :reader smarts
           :initarg smarts)
   (spirit :initform (default-spirit)
           :reader spirit
           :initarg spirit)
   (strength :initform (default-strength)
             :reader strength
             :initarg strength)
   (vigor :initform (default-vigor)
          :reader vigor
          :initarg vigor)
   (charisma :initform 0)
   (pace :initform 6)
   (skills :initarg :skills
           :initform ()
	   :accessor skills)
   (equipment :initform())
   (edges :initform())
   (hindrances :initform())
   (background :initform "")))

(defmethod fighting-value ((record character-record))
  (let ((fight-skill 
	 (find *fighting* (skills record) 
	       :test #'(lambda (l r) (eq (name l) (name r))))))
    (if fight-skill 
	(value (rank fight-skill))
	0)))

(defmethod fighting-modifier ((record character-record))
  (let ((fight-skill 
	 (find *fighting* (skills record) 
	       :test #'(lambda (l r) (eq (name l) (name r))))))
    (if fight-skill 
	(modifier (rank fight-skill))
	0)))


(defmethod shield-bonus ((record character-record))
  0)


(defmethod armor-bonus ((record character-record))
  0)

(defmethod parry ((record character-record))
  (+ 2 
     (half (fighting-value record)) 
     (half (fighting-modifier record))  
     (shield-bonus record) 
     (armor-bonus record)))

(defmethod toughness ((record character-record))
  (+ 2 
     (half (value (rank (vigor record))))
     (half ( modifier (rank (vigor record))))
     (armor-bonus record)))

(defun half (value)
  (/ 2 value))
