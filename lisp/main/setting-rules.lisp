(in-package :savage-worlds)

(defclass setting-rule ()
  ((id :initarg :id 
       :reader id
       :initform (error "Id must be provided"))
   (name :initarg :name 
	 :reader name
	 :initform (error "Name must be provided"))))

(defun setting-rule (&key name)
  (let ((rule (make-instance 'setting-rule :id (list-length *setting-rules*) :name name)))
    (push rule *setting-rules*)
    rule))

(defun find-setting-rules-by-id (setting-rule-ids)
  (remove nil 
	  (map 'list (lambda (setting-rule) 
		       (when (member (id setting-rule) setting-rule-ids) 
			 setting-rule)) 
	       *setting-rules*)))
