(in-package :cl)
(defpackage savage-worlds-asd (:use :cl :asdf))
(in-package :savage-worlds-asd)

(asdf:defsystem #:savage-worlds
    :description "An online virtual table top environment custom to Savage Worlds"
    :version "1.0.0"
    :depends-on (:fiveam 
		 :hunchentoot 
		 :cl-json
		 :cl-store
		 :ht-routes
		 :uuid
		 :cl-stripe)
    :components ((:static-file "README.md")

		 (:module "cl-ddd"
			  :components((:file "package")
				      (:file "authentication-services" :depends-on ("package" 
										    "authentication-entities"
										    "entity"))
				      (:file "authentication-json-endpoint" 
					     :depends-on ( "package" 
							   "authentication-services"))
				      (:file "authentication-entities" :depends-on ("package" 
										    "entity"))
				      (:file "entity" :depends-on ("package"))))

		 (:module "api"
			  :depends-on ( :main :cl-ddd)
			  :components((:file "package")
				      (:file "configuration" :depends-on ("package"))
				      (:file "url-dispatch" :depends-on ("package"))
				      (:file "bootstrap" :depends-on ("package" "configuration"))))


		 (:module "main"
			  :depends-on (:cl-ddd)
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
			  :depends-on (:main )
			  :components ((:file "package")
				       (:file "test-character-record" :depends-on("package"))))))
		 

