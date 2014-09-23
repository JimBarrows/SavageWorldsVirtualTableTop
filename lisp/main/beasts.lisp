(in-package :savage-worlds)

(defvar thick-skin (armor :name 'thick-skin :armor 2 :coverage 'full))
(defvar bite (hand-weapon :name 'bite :damage '(:strength (dice 1 :d 6))))

(deflist :name beast
  :no-list-parent (base-record)
  :parent-slots (agility smarts spirit strength vigor skills pace gear)
  :slots (special-abilities))

(defvar alligator-crocodile (beast :name 'alligator-crocodile
				   :agility (create-agility *d4*)
				   :smarts (create-smarts *d4*)
				   :spirit (create-spirit *d6*)
				   :strength (create-strength *d10*)
				   :vigor (create-vigor *d10*)
				   :skills '((skill fighting *d8*)
					     (skill notice *d6*)
					     (skill swimming *d8*))
				   :pace 3
				   :gear '( thick-skin bite)
				   :special-abilities '('armor-2
							'aquatic-pace-5
							'rollover)))


   
