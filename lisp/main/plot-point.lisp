(in-package :savage-worlds)

(cl-ddd:defentity plot-point()
  ((user-id :initarg :user-id
	    :initform (error "Userid must be provided")
	    :reader user-id
	    :documentation"The user id of the owning user")
   (name :initarg :name
	 :initform (error "Name must be provided")
	 :reader name
	 :documentation "The name of the plot point")))

(defmethod find-all-plot-points-belonging-to ((plot-point-repo plot-point-repository) (user-id uuid:uuid))
  (find-if (lambda (plot-point)
	     (uuid:uuid= user-id (user-id plot-point)))
	   (list-data plot-point-repo)))
	     
