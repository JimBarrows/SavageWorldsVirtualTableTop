(in-package :savage-worlds)

(defun half-of (value)
  (/ value 2))

(defun filter-out (filter-function list)
  (remove nil 
	  (map 'list (lambda (item) 
		       (unless (funcall filter-function item) 
			 item))
	       list)))

(defun filter-in (filter-function list)
  (remove nil 
	  (map 'list (lambda (item) 
		       (when (funcall filter-function item) 
			 item))
	       list)))

(defun each (function-to-apply list)
  (when list
    (remove nil 
	    (map 'list (lambda (item)
			 (funcall function-to-apply item))
		 list))))


(defmacro deflist (&key name (slots '()))
  (let ((list-name (format-symbol t "~:@(~a-list~)" name))
	(find-by-id-list-name (format-symbol t "~:@(find-all-~a-in-id-list~)" name))
	(find-by-id-name (format-symbol t "~:@(find-~a-by-id~)" name))
	(id-name (format-symbol t "~:@(~a-id~)" name))
	(name-name (format-symbol t "~:@(~a-name~)" name)))
    `(progn
       (defclass ,name ()
	 ,(append `(
		    (id :initarg :id 
			:reader ,id-name
			:initform (error "Id must be provided"))
		    (name :initarg :name 
			  :reader ,name-name
			  :initform (error "Name must be provided")))
		  (each (lambda (slot)
			  (if (atom slot)
			      (list slot :initarg (make-keyword (string slot)) 
				    :reader (format-symbol t "~:@(~a-~a~)" name slot))
			      (if (find :accessor slot)
				  slot
				  (append slot (list :initarg  (make-keyword (string (first slot)))
						       :reader (format-symbol t "~:@(~a-~a~)" name (first slot)))))))
			slots)))
       (c2mop:ensure-finalized (find-class ',name))
       (defvar ,list-name '())
       (defun ,name ,(append '(&key name) slots)
	 (let ((,name ,(append `(make-instance ',name 
					       :id (list-length ,list-name) 
					       :name name)
			       (flatten 
				(each (lambda (slot)
					  (if (atom slot)
					      (list
					       (make-keyword (string slot)) 
					       slot)
					      (list
					       (make-keyword (string (first slot))) 
					       (first slot))))
					slots)))))
	   (push ,name ,list-name)
	   ,name))
       (defun ,find-by-id-list-name ( id-list)
	 (filter-in (lambda (,name)
		      (when (member (,id-name ,name) id-list)
			,name))
		    ,list-name))
       (defun ,find-by-id-name (id)
	 (find-if #'(lambda (,name)
		      (= (,id-name ,name) id))
		  ,list-name)))))

(defmacro defsublist (&key name parent (parent-slots '()) (slots '()))
  (let ((list-name (format-symbol t "~:@(~a-list~)" parent)))
    `(progn
       (defclass ,name ( ,parent)
	  ,(each (lambda (slot)
		   (if (atom slot)
		       (list slot :initarg (make-keyword (string slot)) 
			     :reader (format-symbol t "~:@(~a-~a~)" name slot))
		       (if (find :accessor slot)
			   slot
			   (append slot (list :initarg  (make-keyword (string (first slot)))
					      :reader (format-symbol t "~:@(~a-~a~)" name (first slot)))))))
		 slots))
       (defun ,name ,(append '(&key name) 
			      parent-slots
			      slots)
	  (let ((,name ,(append `(make-instance ',name 
						 :id (list-length ,list-name)
						 :name name)
				 (flatten 
				  (each (lambda (slot)
					  (if (atom slot)
					      (list
					       (make-keyword (string slot)) 
					       slot)
					      (list
					       (make-keyword (string (first slot))) 
					       (first slot))))
					(append parent-slots slots))))))
	     (push ,name ,list-name)
	     ,name)))))
