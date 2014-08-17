(in-package :savage-worlds)

(defclass setting-rule ()
  ((id :initarg :id 
       :initform (error "Id must be provided"))
   (name :initarg :name 
	 :initform (error "Name must be provided"))))

(defvar *setting-rules* (list 
		       (make-instance 'setting-rule :id 0 :name "Blood and Guts")
		       (make-instance 'setting-rule :id 1 :name "Born a Hero")
		       (make-instance 'setting-rule :id 2 :name "Critical Failures")
		       (make-instance 'setting-rule :id 3 :name "Fanatics")
		       (make-instance 'setting-rule :id 4 :name "Gritty Damage")
		       (make-instance 'setting-rule :id 5 :name "Heroes Never Die")
		       (make-instance 'setting-rule :id 6 :name "High Adventure")
		       (make-instance 'setting-rule :id 7 :name "Jokers Wild")
		       (make-instance 'setting-rule :id 8 :name "Multiple Languages")
		       (make-instance 'setting-rule :id 9 :name "No Power Points")
		       (make-instance 'setting-rule :id 10 :name "Skill Specialization")))

(defun find-setting-rules-by-id (setting-rule-ids)
  (remove nil 
	  (map 'list (lambda (setting-rule) 
		       (hunchentoot::log-message* :debug "trying ~a type-of first: ~a" (id setting-rule) (type-of (first setting-rule-ids)))
		       (when (member (id setting-rule) setting-rule-ids) 
			 (hunchentoot::log-message* :debug "found ~a" setting-rule)
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
	     
