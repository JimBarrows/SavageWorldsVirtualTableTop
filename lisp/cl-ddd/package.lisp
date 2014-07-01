(in-package :cl-user)

(defpackage cl-ddd
  (:use :cl :cl-store :uuid)
  (:export username-exists-p user defentity users-post))
