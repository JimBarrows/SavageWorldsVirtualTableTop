(in-package :savage-worlds)

(deflist :name gear
  :slots (era
	  weight
	  cost
	  sub-type
	  notes))

(defsublist :name hand-weapon
  :parent gear
  :slots
  (damage
   parry
   two-handed-?
   armor-piercing
   reach))

(defsublist :name armor 
  :parent gear
  :slots
  (armor
   armor-vs-bullets
   armor-pierce
   parry
   coverage))

(defsublist :name ranged-weapon
  :parent gear
  :slots
  (short
   medium
   long
   damage
   rate-of-fire
   shots 
   minimum-strength
   armor-pierce 
   reload 
   revolover-?
   semi-auto-?
   auto-? 
   snapfire-?
   heavy-weapon-?))

(defsublist :name vehicle-mounted-and-at-gun
  :parent gear
  :slots
  (short
   medium
   long
   ap-damage
   ap-ap
   he-damage
   he-ap
   he-template
   rate-of-fire ))

(defsublist :name special-weapon
  :parent gear
  :slots
  (short
   medium
   long
   damage
   rate-of-fire
   armor-piercing
   min-str
   burst))

(defsublist :name ammunition
  :parent gear)
   
(defsublist :name vehicle
  :parent gear
  :slots (acceleration
	  top-speed
	  toughness
	  armor
	  crew
	  passengers))

(defsublist :name aircraft
  :parent gear
  :slots (acceleration
	  top-speed
	  climb
	  toughness
	  armor
	  crew
	  passengers))

(defclass owned-gear ()
  ((total :initform 1
	 :reader total
	 :initarg :total)
   (description :initform (error "Must provide a description")
		:reader description
		:initarg :description)))

(defmethod total-weight ((g gear))
  (* (gear-weight (description g))
     (total g)))

(defmethod name ((g gear))
  (gear-name (description g)))

(defvar dagger (hand-weapon :era 'medieval :name 'dagger :sub-type 'blade :damage '(:strength (dice 1 :d 4)  :weight 1 :cost 25)))

			    
(defvar leather-armor (armor :era 'medieval :name 'leather      :armor 1 :weight 15 :cost 50 :coverage '('torso 'arms 'legs)))
(defvar small-shield  (armor :era 'medieval :name 'small-shield :armor 0 :weight 8  :cost 25 :coverage '('front-left)       :parry 1))

(defvar throwing-axe (ranged-weapon :era 'medieval :name 'throwing-axe :short 3 :medium 6 :long 12 :damage '(:strength :d6) :rate-of-fire 1 :cost 75 :weight 2))

(defvar 25mm-cannon (vehicle-mounted-and-at-gun :name '25mm-cannon :short 25 :medium 50 :long 100 :ap-damage '() :he-damage '((dice 3 :d 8)) :he-ap 4 :rate-of-fire 3 :notes '('heavy-weapon)))

(defvar cannon-shot (special-weapon :name 'cannon-shot :short 50 :medium 100 :long 200 :damage '((dice 3 :d 6) 1) :rate-of-fire 1 :armor-piercing 4 :cost 'military :burst 'medium-bust-template :notes '('heavy-weapon)))

(defvar backpack (gear :sub-type 'adventuring-gear :name 'backpack :cost 50 :weight 2))

(defvar arrow (ammunition :name 'arrow :weight 1/5 :cost 1/2))

(defvar horse-and-carriage (vehicle :name 'horse-and-carriage :toughness 10 :armor 2 :crew 1 :passengers 3 :cost 3000 :notes :see-horse :civilian))

(defvar helicoptor (aircraft :name 'helicopter :acceleration 20 :top-speed 50 :toughness 11 :armor 2 :crew 1  :passengers 3 :cost 500000 :climb -1))
