(in-package :savage-worlds-api)
												
(defmethod json::encode-json((u uuid::uuid) 
			     &optional (stream json::*json-output*)) 
  "encode a uuid class as a string, so we get the actual number"
  (write-char #\" stream)
  (uuid::print-object u stream)
  (write-char #\" stream))

(map-routes
  ("/api/users" :post cl-ddd:users-post 
		:get cl-ddd:users-get )

  ("/api/plotPoints/:id" :put plot-points-put
			 :delete plot-points-delete)

  ("/api/plotPoints" :get plot-points-get
		     :post plot-points-post))

(defconstant +Unprocessable-Entity+ 422)

(hunchentoot::def-http-return-code +Unprocessable-Entity+ 422 "Unprocessable Entity")
