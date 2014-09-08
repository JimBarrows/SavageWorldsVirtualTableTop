(in-package :savage-worlds)

(deflist :name hindrance)

(defsublist :name major-hindrance :parent hindrance)
(defsublist :name minor-hindrance :parent hindrance)
		
(defvar all-thumbs (minor-hindrance :name 'all-thumbs))
(defvar anemic (minor-hindrance :name 'anemic))
(defvar arrogant (major-hindrance :name 'arrogant)) 
(defvar bad-eyes (minor-hindrance :name 'bad-eyes))
(defvar very-bad-eyes (major-hindrance :name 'very-bad-eyes))
