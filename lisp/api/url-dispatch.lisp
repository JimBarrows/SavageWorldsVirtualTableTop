(in-package :savage-worlds-api)
												
(map-routes
  ("/api/users" :post cl-ddd:users-post 
		:get cl-ddd:users-get )

  ("/api/settings/:id" :put settings-put
			 :delete settings-delete)

  ("/api/settings" :get settings-get
		     :post settings-post)

  ("/api/settingRules/:id" :get setting-rules-get-one)

  ("/api/settingRules" :get setting-rules-get-all)

  ("/api/skillDescriptions" :get skill-descriptions-get)

  ("/api/hindrances/:id" :get hindrances-get-one)

  ("/api/hindrances" :get hindrances-get-all))
