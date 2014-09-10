(in-package :savage-worlds-api)

(setf hunchentoot::*show-lisp-errors-p* t)
(setf *catch-errors-p* nil)
(setf *lisp-errors-log-level* :info)
(setf *lisp-warnings-log-level* :info)
(defvar *http-stream* *standard-output*)

(defvar *project-name* "Savage Worlds")

(defconstant +Unprocessable-Entity+ 422)

(hunchentoot::def-http-return-code +Unprocessable-Entity+ 422 "Unprocessable Entity")

(defvar *port* 8080)
