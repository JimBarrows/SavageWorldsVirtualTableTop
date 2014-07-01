(in-package :savage-worlds-api)
												
(defmethod json::encode-json((u uuid::uuid) 
			     &optional (stream *json-output*)) 
  "encode a uuid class as a string, so we get the actual number"
  (write-char #\" stream)
  (uuid::print-object u stream)
  (write-char #\" stream))

(map-routes
  ("/users" :post users-post )
)

(defconstant +Unprocessable-Entity+ 422)

(hunchentoot::def-http-return-code +Unprocessable-Entity+ 422 "Unprocessable Entity")
