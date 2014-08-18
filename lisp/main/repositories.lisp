(in-package :savage-worlds)

(defvar *setting-repository* nil)
(defvar *user-repository* nil)

(defun load-repositories()
  (setf *setting-repository* (make-instance 'setting-repository))
  (cl-ddd::load-data *setting-repository*)
  (load-setting-rule-list)
  (load-skill-description-list))

(defun unload-repositories()
  (cl-ddd::save-data *setting-repository*))
  
