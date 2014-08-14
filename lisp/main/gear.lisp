(in-package :savage-worlds)

(defclass gear-description ()
  ((name :initarg :name
	 :initform (error "Must provide a name for the gear")
	 :reader name)
   (weight :initarg :weight
		 :initform 0
		 :reader weight)
   (cost :initarg :cost
	 :initform 0
	 :reader cost)))

(defclass hand-weapon-description ( gear-description)
  ((era :initform (error "Must provide an era")
	:reader era
	:documentation "One of Medieval, Modern, or Futuristic, settings can add their own")
   (damage :initform (error "Must provide damage")
	   :reader damage
	   :documentation "For hand weapons this should be a rank value")
   (parry :initform 0
	  :reader parry)
   (two-hands :initform nil
		:reader is-two-handed)
   (armor-pierce :initform nil
		 :reader armor-pierce)
   (reach :initform nil
	  :reader has-reach)))

(defclass armor (gear-description)
  ((era :initform (error "Must provide an era")
	:reader era
	:initarg :era
	:documentation "One of Medieval, Modern, or Futuristic, settings can add their own")
  (armor :initform 1
	 :reader armor
	 :initarg :armor)
   (armor-pierce :initform nil
		 :reader armor-pierce
		 :initarg :ap)
   (parry :initform nil
	  :initarg :parry
	  :reader parry)
   (coverage :initform '(torso)
	     :initarg :coverage
	     :reader covers)))

(defclass kevlar(amor)
  ((armor-vs-bullets :initform 0
		     :initarg :vs-bullets
		     :reader :armor-vs-bullets)))

(defclass barding(armor)())

(defclass shield(armor)())

(defclass ranged-weapons (gear-description)
  ((era :initform (error "Must provide an era")
	:reader era
	:documentation "One of Medieval, Modern, or Futuristic, settings can add their own")
   (short :initform (error "Must provide a short range value")
	  :reader short
	  :initarg :s)
   (medium :initform (error "Must provide a medium range value")
	   :reader medium
	   :initarg :m)
   (long :initform (error "Must provide a long range value")
	 :reader long
	 :initarg :l)
   (damage :initform (error "Must provide a damage value")
	   :reader damage
	   :initarg :damage
	   :documentation "Can be either a dice value, or trait-rank")
   (rate-of-fire :initform 1
		 :reader rate-of-fire
		 :initarg :rof)
   (shots :initform nil
	  :reader shots
	  :initarg :shots)
   (minimum-strength :initform nil
		     :reader minimum-strength
		     :initarg :min-str)
   (armor-pierce :initform nil
		 :reader armor-pierce)
   (reload :initform 0
	   :reader ranged-weapon-reload
	   :initarg :actions-to-reload)
   (revolover-p :initform nil
		:reader ranged-weapon-is-revolver
		:initarg :revolver)
   (semi-auto-p :initform nil
		:reader ranged-weapon-is-semi-auto
		:initarg :semi-auto)
   (auto-p :initform nil
	   :reader ranged-weapon-is-automatic
	   :initarg :auto)
   (snapfire :initform nil
	     :reader ranged-weapon-is-snapfire-capable
	     :initarg :snapfire)
   (heavy-weapon-p :initform nil
		   :reader is-heavy-weapon
		   :initarg :hw)))

(defclass gear ()
  ((total :initform 1
	 :reader total
	 :initarg :total)
   (description :initform (error "Must provide a description")
		:reader description
		:initarg :description)))

(defmethod total-weight ((g gear))
  (* (weight (description g))
     (total g)))

(defmethod name ((g gear))
  (name (description g)))

(defvar leather-armor (make-instance 'armor :era 'medieval
				     :name 'leather
				     :armor 1
				     :weight 15
				     :cost 50
				     :coverage '('torso 'arms 'legs)))

(defvar small-shield (make-instance 'shield :era 'medieval 
				    :name 'small-shield 
				    :armor 0 
				    :weight 8 
				    :cost 25 
				    :parry 1))


