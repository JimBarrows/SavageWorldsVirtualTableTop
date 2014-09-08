(in-package :savage-worlds-api)
												
(map-routes
  ("/api/users" :post cl-ddd:users-post 
		:get cl-ddd:users-get )

  ("/api/settings/:id" :put settings-put
			 :delete settings-delete)

  ("/api/settings" :get settings-get
		     :post settings-post)

  ("/api/settingRules/:id" :get setting-rules-get-by-id)

  ("/api/settingRules" :get setting-rules-get-all)

  ("/api/skillDescriptions" :get skill-descriptions-get-all)

  ("/api/skillDescriptions/:id" :get skill-descriptions-get-by-id)

  ("/api/hindrances/:id" :get hindrances-get-by-id)

  ("/api/hindrances" :get hindrances-get-all)

  ("/api/edges/:id" :get edges-get-by-id)

  ("/api/edges" :get edges-get-all)

  ("/api/gear/:id" :get gear-get-by-id)

  ("/api/gear" :get gear-get-all))

