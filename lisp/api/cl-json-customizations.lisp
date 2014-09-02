(in-package json)

(defmethod encode-json((setting savage-worlds::setting) 
		       &optional (stream json::*json-output*)) 
 "Encode a setting"
  (format (or stream nil) "{ \"id\": \"~a\", \"userId\": \"~a\", \"name\": \"~a\", \"settingRules\": [~{~a~^, ~}], \"skillDescriptions\":[~{~a~^, ~}], \"hindrances\":[~{~a~^, ~}], \"edges\":[~{~a~^, ~}]}"
	  (cl-ddd::id setting)
	  (savage-worlds::user-id setting)
	  (savage-worlds::name setting)
	  (savage-worlds::setting-rules setting)
	  (savage-worlds::skill-descriptions setting)
	  (savage-worlds::hindrances setting)
	  (savage-worlds::edges setting)))

(defmethod encode-json((u uuid::uuid) 
		       &optional (stream json::*json-output*)) 
  "encode a uuid class as a string, so we get the actual number"
  (write-char #\" stream)
  (uuid::print-object u stream)
  (write-char #\" stream))

(defun array-of-ids (entity-list)
  "Extract a comma seperate list of ids from the list provided"
  (let ((id-list (map 'list 
		      (lambda (entity)
			(cl-ddd::id entity))
		      entity-list)))
    (format nil "~{~a~^, ~}" id-list)))
