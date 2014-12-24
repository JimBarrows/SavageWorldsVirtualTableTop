/**
 * 
 */

App.AlertController = Ember.Controller.extend({
	alert : false,
	observeAlert : function() {
		if (this.alert != false) {
			$('#flash').addClass(
					'alert alert-' + this.alert[0] + ' alert-dismissable');
			$('#flash span').text(this.alert[1]);
			$('#flash').fadeIn();
		} else {
			$('#flash').hide();
		}
	}.observes('alert')
});

App.SkilldescriptionsCreateController = Ember.Controller.extend({
	needs : [ 'alert' ],
	attributeTypes : ["Agility", "Smarts", "Strength", "Spirit", "Vigor"],
	actions : {
		save : function() {
			return this.model.save().then(function(){
				  route.transitionTo('skilldescriptions');
			}, function() {
			  // Couldn't save, do nothing about it.
			});
		}
	}
});

App.SkilldescriptionsEditController = Ember.Controller.extend({
	needs : [ 'alert' ],
	attributeTypes : ["Agility", "Smarts", "Strength", "Spirit", "Vigor"],
	actions : {
		save : function() {
			return this.model.save().then(function(){
				  route.transitionTo('skilldescriptions');
			}, function() {
			  // Couldn't save, do nothing about it.
			});
		}
	}
});