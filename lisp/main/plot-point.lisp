(in-package :savage-worlds)

(cl-ddd:defentity plot-point()
  ((user-id :initarg :user-id
	    :initform (error "Userid must be provided")
	    :documentation"The user id of the owning user")
   (name :initarg :name
	 :initform (error "Name must be provided")
	 :documentation "The name of the plot point")))

(defmethod update ((repo plot-point-repository) (id uuid::uuid) &key (name))
  (let ((original (cl-ddd::find-by-id repo id)))
    (setf (name original) name)
    original))

(defmethod find-all-plot-points-belonging-to ((plot-point-repo plot-point-repository) (user-id uuid:uuid))
  (mapcar (lambda (plot-point)
	     (uuid:uuid= user-id (user-id plot-point)))
	   (cl-ddd::list-data plot-point-repo)))
	     
