(in-package :cl)
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
				       (:file "utils" 
					      :depends-on ("package"))
				       (:file "traits"
					      :depends-on ("package"))
				       (:file "dice" :depends-on ("package"))
				       (:file "races" :depends-on ("package"))
				       (:file "gear" :depends-on ("package"))
				       (:file "skills" 
					      :depends-on ("package" 
							   "traits" 
							   "dice"))
				       (:file "edges"
					      :depends-on( "package"
							    "traits"))
				       (:file "hindrances"
					      :depends-on( "package"
							   "traits"))
				       (:file "plot-point" :depends-on ("package"))
				       (:file "character-record" 
					      :depends-on ("package" 
							   "utils"
							   "traits" 
							   "dice" 
							   "races" 
							   "skills"
							   "gear"
							   "edges"
							   "traits"))
				       (:file "character-creation-state"
					      :depends-on("package"
							  "character-record"))))
		 (:module "unit-tests"
			  :pathname "src/test/unit/lisp"
			  :depends-on (:main )
			  :components ((:file "package")
				       (:file "test-character-record" :depends-on("package"))))))
		 

