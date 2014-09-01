export default{
    name: 'session',
    after: 'authentication',
    initialize: function(container/*, application*/){
	var applicationRoute = container.lookup('route:application');
	var session  = container.lookup('simple-auth-session:main');
	// handle the session events
	session.on('sessionAuthenticationSucceeded', function() {
	    applicationRoute.transitionTo('member');
	});
    }
};

