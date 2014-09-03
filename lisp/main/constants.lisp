(in-package :savage-worlds)

(defvar *setting-rules* '())

(defvar blood-and-guts       (setting-rule :name 'Blood-and-Guts))
(defvar born-a-hero          (setting-rule :name 'Born-a-Hero))
(defvar critical-failures    (setting-rule :name 'Critical-Failures))
(defvar fanatics             (setting-rule :name 'Fanatics))
(defvar gritty-damage        (setting-rule :name 'Gritty-Damage))
(defvar heroes-never-die     (setting-rule :name 'Heroes-Never-Die))
(defvar high-adventure       (setting-rule :name 'High-Adventure))
(defvar jokers-wild          (setting-rule :name 'Jokers-Wild))
(defvar multiple-languages   (setting-rule :name 'Multiple-Languages))
(defvar no-power-points      (setting-rule :name 'No-Power-Points))
(defvar skill-specialization (setting-rule :name 'Skill-Specialization))

(defvar *edges* '())

(defvar attractive (background-edge :name 'attractive ))
(defvar very-attractive (background-edge :name 'very-attractive))

(defvar *hindrances* '())

(defvar all-thumbs (minor-hindrance :name 'all-thumbs))
(defvar anemic (minor-hindrance :name 'anemic))
(defvar arrogant (major-hindrance :name 'arrogant)) 
(defvar bad-eyes (minor-hindrance :name 'bad-eyes))
(defvar very-bad-eyes (major-hindrance :name 'very-bad-eyes))

(defvar *skill-descriptions* '())

(defvar *boating*        (skill-description :name 'boating :attribute 'agility))
(defvar *climbing*       (skill-description :name 'climbing :attribute 'strength))
(defvar *driving*        (skill-description :name 'driving :attribute 'agility))
(defvar *fighting*       (skill-description :name 'fighting :attribute 'agility))
(defvar *gambling*       (skill-description :name 'gambling :attribute 'smarts))
(defvar *healing*        (skill-description :name 'healing :attribute 'smarts))
(defvar *intimidation*   (skill-description :name 'intimidation :attribute 'spirit))
(defvar *investigation*  (skill-description :name 'investigation :attribute 'smarts))
