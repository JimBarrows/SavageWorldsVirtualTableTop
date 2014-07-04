App.LoginController = Ember.ObjectController.extend({

    confirm_password: "",

    actions: {
	signup: function() {
	    var user = this.get('model')
	    var dis = this
	    user.save().then( function( post) {
		dis.transitionToRoute('/tasks', post)
	    })
	}
    }
})
