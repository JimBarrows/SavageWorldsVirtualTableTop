(in-package :savage-worlds)

(deflist :name skill-description :slots (attribute))

(defvar boating        (skill-description :name 'boating :attribute 'agility))
(defvar climbing       (skill-description :name 'climbing :attribute 'strength))
(defvar driving        (skill-description :name 'driving :attribute 'agility))
(defvar fighting       (skill-description :name 'fighting :attribute 'agility))
(defvar gambling       (skill-description :name 'gambling :attribute 'smarts))
(defvar healing        (skill-description :name 'healing :attribute 'smarts))
(defvar intimidation   (skill-description :name 'intimidation :attribute 'spirit))
(defvar investigation  (skill-description :name 'investigation :attribute 'smarts))
