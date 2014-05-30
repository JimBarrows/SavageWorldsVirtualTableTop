l(in-package :cl)
(defpackage savage-worlds-asd (:use :cl :asdf))
(in-package :savage-worlds-asd)

(asdf:defsystem #:savage-worlds
    :description "An online virtual table top environment custom to Savage Worlds"
    :version "1.0.0"
    :depends-on (:fiveam)
    :components ((:static-file "README.md")
		 (:module "main"
			  :pathname "src/main/lisp"
			  :components ((:file "package")
				       (:file "traits")
				       (:file "dice")
				       (:file "races")
				       (:file "character-record" 
					      :depends-on ("package" 
							   "traits" 
							   "dice" 
							   "races" 
							   "skills"))
				       (:file "plot-point" :depends-on ("package"))
				       (:file "skills" 
					      :depends-on ("package" 
							   "traits" 
							   "dice"))))
		 (:module "unit-tests"
			  :pathname "src/test/unit/lisp"
			  :depends-on (:main )
			  :components ((:file "package")
				       (:file "test-character-record" :depends-on("package"))))))
		 

