export default Ember.ObjectController.extend({

    confirm_password: "",
    confirm_password_error: "",

    validPasswords : function( password, confirm) {
	if( confirm === ""){
	    this.set("confirm_password_error", "Cannot be blank, and must match the password above");
	    return false;
	} else if ( confirm !== password) {
	    this.set("confirm_password_error", "Cannot be blank, and must match the password above");
	    return false;
	} else {
	    return true;
	}
    },


    actions: {
	signup: function() {
	    var user = this.get('model');
	    var self = this;
	    var cp = self.get("confirm_password");
	    if( this.validPasswords( user.get('password'), cp)){		
		user.save().then( 
		    function( post) {
			self.transitionToRoute('/member', post);
			self.set("confirm_password", "");
		    },
		    function(error){
		    });
	    }
	}
    }
});
