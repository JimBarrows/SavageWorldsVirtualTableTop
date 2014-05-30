(in-package :savage-worlds)

(defclass trait-rank ()
  ((rank
    :initarg :rank
    :initform :d4
    :reader rank)
   (modifier 
    :initarg :modifier
    :initform 0
    :reader modifier)
   (value
    :initarg :value
    :initform 4
    :reader value)))

(defvar *d4* (make-instance 'trait-rank))
(defvar *d6* (make-instance 'trait-rank :rank :d6 :value 6))
(defvar *d8* (make-instance 'trait-rank :rank :d8 :value 8))
(defvar *d10* (make-instance 'trait-rank :rank :d10 :value 10))
(defvar *d12* (make-instance 'trait-rank :rank :d12 :value 12))
