(in-package :cl-user)

(defpackage cl-ddd
  (:use :cl :cl-store :uuid :cl-json)
  (:export username-exists-p user defentity signup users-post users-get))
