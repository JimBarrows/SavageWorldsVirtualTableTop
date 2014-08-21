(push #p"../lisp/" asdf:*central-registry*)
(ql:quickload "savage-worlds")
(defpackage build
  (:use :cl :cl-semver)
  (:export build main release-patch release-minor release-major))

(in-package :build)

(defvar version (or (cl-semver::load-version)
		    (make-instance 'version)))

(defun main()
  (savage-worlds-api:start-application)
  (loop))

(defun save-core (core-fn)
  (progn
    #+sbcl
    (let ((fork-result (sb-posix:fork)))
      (case fork-result
	(-1 (error "fork failed"))
	(0 (sb-ext:save-lisp-and-die core-fn :toplevel #'build:main :executable t))
	(otherwise (sb-posix:wait)))
      (format t "stand-alone core ~a saved" core-fn))
    #-sbcl
    (error "not available on this lisp")
    (values)))

(defun build()
  (save-core (format nil "../build/savage-worlds-~a" (as-string version))))

(defun release-patch()
  (build)
  (cl-semver::save-version (increment-patch version)))

(defun release-minor()
  (build)
  (cl-semver::save-version (increment-minor version)))

(defun release-major()
  (build)
  (cl-semver::save-version (increment-major version)))
