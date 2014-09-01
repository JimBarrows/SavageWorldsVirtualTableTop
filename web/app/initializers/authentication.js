import Base from 'simple-auth/authenticators/base';
import Ember from 'ember';

var CustomAuthenticator = Base.extend({
    authenticate: function(credentials) {
	var self = this;
	return new Ember.RSVP.Promise( function (resolve, reject){
	    self.get('store').find( 'user', {
		username: credentials.identification,
		password: credentials.password		
	    }).then( function (user) {
		if( user.get('content').length) {
		    var username = user.get('content').objectAt(0).get('data').username;
		    var userId= user.get('content').objectAt(0).get('data').id;
		    resolve({username: username,
			    userId: userId});
		} else {
		    reject();
		}
	    });
	});
    },

    invalidate: function(data) {
	data = data; //is not used
	return new Ember.RSVP.resolve();
    },

    restore: function(data) {
	data = data; //is not used
	return new Ember.RSVP.reject();
    }
});

export default {
    name: 'authentication',
    before: 'simple-auth',
    initialize: function(container, application) {
	application = application; //is not used
	container.register('authenticator:custom', CustomAuthenticator);
	container.injection('authenticator:custom', 'store', 'store:main');
    }
};
