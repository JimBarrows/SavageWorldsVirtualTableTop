(in-package :cl-user)

(defpackage savage-worlds-api
	(:use :cl :hunchentoot :cl-store :cl-json :cl-ddd :ht-routes :cl-ddd :savage-worlds)
	(:export start-application stop-application))
