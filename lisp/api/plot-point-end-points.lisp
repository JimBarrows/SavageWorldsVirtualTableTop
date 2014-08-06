(in-package :savage-worlds-api)

(defun plot-points-get ()
  (hunchentoot::log-message* :debug "plot-points-get")
  (setf (hunchentoot:content-type*) "application/json") 
  (let ((plot-point-list (cl-ddd::list-data savage-worlds::*plot-point-repository*)))
    (if plot-point-list
	(format nil "{\"plot-point\":~a}" (encode-json-to-string 
					   (cl-ddd::list-data  savage-worlds::*plot-point-repository*)))
	(format nil "{\"plot-point\":[]}"))))

(defun plot-points-post ()
  (setf (hunchentoot:content-type*) "application/json") 
  (let* ((input-string (hunchentoot::raw-post-data :force-text t))
	 (input-json (rest (first (decode-json-from-string input-string))))
	 (name (string-trim " " (rest (assoc :name input-json))))
	 (user-id (uuid:make-uuid-from-string (string-trim " " (rest (assoc :user-id input-json)))))
	 (new-plot-point (make-instance savage-worlds::'plot-point :name name :user-id user-id)))
    (hunchentoot::log-message* :debug "user-id ~a; name ~a" user-id name)
    (cl-ddd::add savage-worlds::*plot-point-repository* new-plot-point)
    (format nil "{\"plot-point\":~a}" (encode-json-to-string new-plot-point))))
