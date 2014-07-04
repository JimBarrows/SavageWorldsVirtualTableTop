export default Ember.ObjectController.extend({

    confirm_password: "",
    confirm_password_error: "",
    credit_card_number: "",
    cvc_number: "",
    expiration_date: "",
    credit_card_error: false,
    credit_card_error_message: "",

    validPasswords : function( password, confirm) {
	if( confirm === ""){
	    this.set("confirm_password_error", "Cannot be blank, and must match the password above")
	    return false
	} else if ( confirm !== password) {
	    this.set("confirm_password_error", "Cannot be blank, and must match the password above")
	    return false
	} else {
	    return true
	}
    },

    validCreditCard : function( creditCardNumber, cvc, expirationDate) {
	var retVal = true
	if( ! Stripe.card.validateCardNumber(creditCardNumber)) {
	    this.set("credit_card_number_error", "Credit Card number is not valid")
	    retVal = false
	}
	if( ! Stripe.card.validateCVC(cvc)){
	    this.set("cvc_number_error", "Cannot be blank")
	    retVal = false
	}
	if( expirationDate ==="") {
	    this.set("expiration_date_error", "Cannot be blank")
	    retVal = false
	}else {
	    var month = expirationDate.split("/")[0]
	    var year = expirationDate.split("/")[1]
	    if( !Stripe.card.validateExpiry( month, year)) {
		this.set("expiration_date_error", "Expiration date is invalid")
		retVal = false
	    }
	}
	
	return retVal
    },

    stripeResponseHandler:function stripeResponseHandler(status, response) {
	
	if (response.error) {
	    self.set("credit_card_error", true)
	    self.set("credit_card_error_message", response.error.message)
	} else {
	   self.get('model').set('stripe_token', response.id)
	}
    },

    actions: {
	signup: function() {
	    var user = this.get('model')
	    var self = this
	    var cp = self.get("confirm_password")
	    var ccn = self.get("credit_card_number")
	    var cvc = self.get("cvc_number")
	    var expirationDate = self.get("expiration_date")
	    var password = user.get('password')
	    if( this.validPasswords( password, cp)
		&& this.validCreditCard( ccn, cvc, expirationDate)) {
		var expirationMonth = expirationDate.split("/")[0]
		var expirationYear = expirationDate.split("/")[1]
		Stripe.card.createToken({
		    number: ccn,
		    cvc: cvc,
		    exp_month: expirationMonth,
		    exp_year: expirationYear
		}, function( status, response) {
		    if (response.error) {
			self.set("credit_card_error", true)
			self.set("credit_card_error_message", response.error.message)
		    } else {
			user.set('stripe_token', response.id)
			user.save().then( 
			    function( post) {
				self.transitionToRoute('/tasks', post)
			    },
			    function(error){
				
			    }
			)
		    }
		});
	    }
	    this.set("confirm_password", "")
	}
    }
})
