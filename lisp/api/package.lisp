(in-package :cl-user)

(defpackage savage-worlds-api
	(:use :cl :hunchentoot :cl-store :cl-json :cl-ddd :ht-routes :savage-worlds)
	(:export start-application stop-application))
