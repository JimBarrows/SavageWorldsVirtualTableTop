(in-package :savage-worlds)

(defclass setting-rule ()
  ((id :initarg :id 
       :reader id
       :initform (error "Id must be provided"))
   (name :initarg :name 
	 :reader name
	 :initform (error "Name must be provided"))))

(defvar *setting-rules* '())

(deffun setting-rule (&key name)
  (let ((rule (make-instance 'setting-rule :id (list-length *setting-rules*) :name name)))
    (push rule *setting-rules*)
    setting-rule))


(defvar blood-and-guts       (setting-rule :name Blood-and-Guts))
(defvar born-a-hero          (setting-rule :name Born-a-Hero))
(defvar critical-failures    (setting-rule :name Critical-Failures))
(defvar fanatics             (setting-rule :name Fanatics))
(defvar gritty-damage        (setting-rule :name Gritty-Damage))
(defvar heroes-never-die     (setting-rule :name Heroes-Never-Die))
(defvar high-adventure       (setting-rule :name High-Adventure))
(defvar jokers-wild          (setting-rule :name Jokers-Wild))
(defvar multiple-languages   (setting-rule :name Multiple-Languages))
(defvar no-power-points      (setting-rule :name No-Power-Points))
(defvar skill-specialization (setting-rule :name Skill-Specialization))

(defun find-setting-rules-by-id (setting-rule-ids)
  (remove nil 
	  (map 'list (lambda (setting-rule) 
		       (when (member (id setting-rule) setting-rule-ids) 
			 setting-rule)) 
	       *setting-rules*)))
