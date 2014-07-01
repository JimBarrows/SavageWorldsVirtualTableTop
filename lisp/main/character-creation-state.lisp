(in-package :savage-worlds)

(defclass character-creation-state ()
  ((record :initform (make-instance 'character-record)
	  :initarg record
	  :reader record)
   (history :initform ()
	    :reader history)
   (attribute-points :initform 5
		     :reader attribute-points)
   (skill-points :initform 15
		 :reader skill-points)
   (hindrance-points :initform 0
		     :reader hindrance-points)
   (max-number-major-hindrances :initform 1
			  :reader max-number-major-hindrances)
   (max-number-minor-hindrances :initform 1
			  :reader max-number-minor-hindrances)))

;(defmethod increase-strength ((state character-creation-state))
 ; (let ((record (record state))
;	(attribute-points (attribute-points state)))
 ;   (if (< 0 attribute-points)
;	(progn
;	  (push record (
	
	  
