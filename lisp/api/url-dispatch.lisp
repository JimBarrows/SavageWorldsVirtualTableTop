(in-package :savage-worlds-api)
												
(map-routes
  ("/api/users" :post cl-ddd:users-post 
		:get cl-ddd:users-get )

  ("/api/plotPoints/:id" :put plot-points-put
			 :delete plot-points-delete)

  ("/api/plotPoints" :get plot-points-get
		     :post plot-points-post)

  ("/api/settingRules" :get setting-rules-get))

(defconstant +Unprocessable-Entity+ 422)

(hunchentoot::def-http-return-code +Unprocessable-Entity+ 422 "Unprocessable Entity")
