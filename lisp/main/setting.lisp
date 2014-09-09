(in-package :savage-worlds)


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
		       :documentation "List of skills available for this setting.")
   (hindrances :initarg :hindrances
		       :initform '()
		       :documentation "List of hindrances available for this setting.")
   (edges :initarg :edges
		       :initform '()
		       :documentation "List of edges available for this setting.")
   (gear-list :initarg :gear
		       :initform '()
		       :documentation "List of gear available for this setting.")))

(defmethod update ((repo setting-repository) 
		   (id uuid::uuid) 
		   &key 
		     (name) 
		     (setting-rule-ids)
		     (skill-descriptions)
		     (hindrances)
		     (edges)
		     (gear))
  (let ((original (cl-ddd::find-by-id repo id)))
    (setf (name original) name)
    (setf (setting-rules original)  setting-rule-ids)
    (setf (skill-descriptions original) skill-descriptions)
    (setf (hindrances original) hindrances)
    (setf (edges original) edges)
    (setf (gear-list original) gear)
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
	     
