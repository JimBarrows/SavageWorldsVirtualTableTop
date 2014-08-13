(in-package :savage-worlds)

(defentity setting-rule ()
  ((id :initarg :id 
       :initform (error "Id must be provided"))
   (name :initarg :name 
	 :initform (error "Name must be provided"))))

(setf *setting-rules* (list 
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
  (hunchentoot::log-message* :debug "find setting rules - setting-rule-ids: ~a" setting-rule-ids)
  (remove nil 
	  (map 'list (lambda (setting-rule) 
		       (hunchentoot::log-message* :debug "trying ~a type-of first: ~a" (id setting-rule) (type-of (first setting-rule-ids)))
		       (when (member (id setting-rule) setting-rule-ids) 
			 (hunchentoot::log-message* :debug "found ~a" setting-rule)
			 setting-rule)) 
	       *setting-rules*)))

(cl-ddd:defentity plot-point()
  ((user-id :initarg :user-id
	    :initform (error "Userid must be provided")
	    :documentation"The user id of the owning user")
   (name :initarg :name
	 :initform (error "Name must be provided")
	 :documentation "The name of the plot point")
   (setting-rules :initarg :setting-rules
		  :initform '()
		  :documentation "List of rules for this setting")))

(defmethod update ((repo plot-point-repository) 
		   (id uuid::uuid) 
		   &key 
		     (name) 
		     (setting-rule-ids))
  (let ((original (cl-ddd::find-by-id repo id)))
    (setf (name original) name)
    (hunchentoot::log-message* :debug "find setting rules: ~a" (find-setting-rules-by-id setting-rule-ids))
    (setf (setting-rules original)  setting-rule-ids)
    original))

(defmethod delete-plot-point ((repo plot-point-repository) (id uuid::uuid))
  (let ((original (cl-ddd::find-by-id repo id)))
    (setf (cl-ddd::data repo)
	  (remove original (cl-ddd::list-data repo)))))


(defmethod find-all-plot-points-belonging-to ((plot-point-repo plot-point-repository) 
					      (user-id uuid:uuid))
  (remove nil 
	  (map 'list (lambda (plot-point)
		       (when (uuid:uuid= user-id (user-id plot-point))
			 plot-point))
	       (cl-ddd::list-data plot-point-repo))))
	     
