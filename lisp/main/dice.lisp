(in-package :savage-worlds)

(defclass dice ()
  ((number-of-times-to-roll :initarg :number-of-times-to-roll
	  :initform 1
	  :reader number-of-times-to-roll)
   (sides :initarg :sides
	  :initform 6
	  :reader sides)))

(defun dice ( number-of-times-to-roll &key (d 6) )
  (make-instance 'dice :number-of-times-to-roll number-of-times-to-roll :sides d))


