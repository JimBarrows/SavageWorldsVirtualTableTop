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

(defvar *human* "Human")

(deftype attributes () '(member agility smarts spirit strength vigor))

(defclass trait () 
  ((name
    :initarg :name
    :reader name)
   (rank
    :initarg :rank
    :initform *d4*
    :reader rank)))

(defclass attribute( trait)
  ())

(defclass skill( trait)
  (description :initarg :description))

(defclass character-record ()
  ((name
    :accessor name)
   (race :initform *human*
         :initarg :race
         :reader race)
   (agility :initform (make-instance 'attribute)
            :reader agility
            :initarg :agility)
   (smarts :initform (make-instance 'attribute)
           :reader smarts
           :initarg smarts)
   (spirit :initform (make-instance 'attribute)
           :reader spirit
           :initarg spirit)
   (strength :initform (make-instance 'attribute)
             :reader strength
             :initarg strength)
   (vigor :initform (make-instance 'attribute)
          :reader vigor
          :initarg vigor)
   (charisma :initform 0)
   (pace :initform 6)
   (skills :initarg :skills
           :initform ())
   (equipment :initform())
   (edges :initform())
   (hindrances :initform())
   (background :initform "")))

(defmethod fighting-value ((record character-record))
  0)

(defmethod fighting-modifier ((record character-record))
  0)


(defmethod shield-bonus ((record character-record))
  0)

(defmethod armor-bonus ((record character-record))
  0)

(defmethod parry ((record character-record))
  (+ 2 ( / 2 (fighting-value record)) 
     (/ 2 (fighting-modifier record))  
     (shield-bonus record) 
     (armor-bonus record)))

(defmethod toughness ((record character-record))
  (+ 2 
     (/ 2 (vigor record)) 
     (/ 2 ( modifier (vigor record)))
     (armor-bonus record)))
