(in-package :savage-worlds)

(defclass setting-rule ()
  ((id :initarg :id 
       :reader id
       :initform (error "Id must be provided"))
   (name :initarg :name 
	 :reader name
	 :initform (error "Name must be provided"))))


(defvar *blood-and-guts*       (make-instance 'setting-rule :id 0 :name "Blood and Guts"))
(defvar *born-a-hero*          (make-instance 'setting-rule :id 1 :name "Born a Hero"))
(defvar *critical-failures*    (make-instance 'setting-rule :id 2 :name "Critical Failures"))
(defvar *fanatics*             (make-instance 'setting-rule :id 3 :name "Fanatics"))
(defvar *gritty-damage*        (make-instance 'setting-rule :id 4 :name "Gritty Damage"))
(defvar *heroes-never-die*     (make-instance 'setting-rule :id 5 :name "Heroes Never Die"))
(defvar *high-adventure*       (make-instance 'setting-rule :id 6 :name "High Adventure"))
(defvar *jokers-wild*          (make-instance 'setting-rule :id 7 :name "Jokers Wild"))
(defvar *multiple-languages*   (make-instance 'setting-rule :id 8 :name "Multiple Languages"))
(defvar *no-power-points*      (make-instance 'setting-rule :id 9 :name "No Power Points"))
(defvar *skill-specialization* (make-instance 'setting-rule :id 10 :name "Skill Specialization"))

(defvar *setting-rules* '())

(defun load-setting-rule-list()
  (setf *setting-rules* '())
  (push *blood-and-guts* *setting-rules*)
  (push *born-a-hero* *setting-rules*)
  (push *critical-failures* *setting-rules*)
  (push *fanatics* *setting-rules*)
  (push *gritty-damage* *setting-rules*)
  (push *heroes-never-die* *setting-rules*)
  (push *high-adventure* *setting-rules*)
  (push *jokers-wild* *setting-rules*)
  (push *multiple-languages* *setting-rules*)
  (push *no-power-points* *setting-rules*)
  (push *skill-specialization* *setting-rules*))

(defun find-setting-rules-by-id (setting-rule-ids)
  (remove nil 
	  (map 'list (lambda (setting-rule) 
		       (when (member (id setting-rule) setting-rule-ids) 
			 setting-rule)) 
	       *setting-rules*)))

(cl-ddd:defentity setting()
  ((user-id :initarg :user-id
	    :initform (error "Userid must be provided")
	    :documentation"The user id of the owning user")
   (name :initarg :name
	 :initform (error "Name must be provided")
	 :documentation "The name of the setting")
   (setting-rules :initarg :setting-rules
		  :initform '()
		  :documentation "List of rules for this setting")
   (skill-descriptions :initarg :skill-descriptions
		       :initform '()
		       :documentation "List of skills available for this setting.")))

(defmethod update ((repo setting-repository) 
		   (id uuid::uuid) 
		   &key 
		     (name) 
		     (setting-rule-ids)
		     (skill-descriptions))
  (let ((original (cl-ddd::find-by-id repo id)))
    (setf (name original) name)
    (setf (setting-rules original)  setting-rule-ids)
    (setf (skill-descriptions original) skill-descriptions)
    original))

(defmethod delete-setting ((repo setting-repository) (id uuid::uuid))
  (let ((original (cl-ddd::find-by-id repo id)))
    (setf (cl-ddd::data repo)
	  (remove original (cl-ddd::list-data repo)))))


(defmethod find-all-settings-belonging-to ((setting-repo setting-repository) 
					      (user-id uuid:uuid))
  (remove nil 
	  (map 'list (lambda (setting)
		       (when (uuid:uuid= user-id (user-id setting))
			 setting))
	       (cl-ddd::list-data setting-repo))))
	     
