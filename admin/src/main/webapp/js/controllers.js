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

App.SkilldescriptionsIndexController = Ember.Controller.extend({
	actions : {
		remove : function(skill) {
			skill.deleteRecord();
			return skill.save();
		}
	}
});

App.SkilldescriptionsCreateController = Ember.Controller.extend({
	needs : [ 'alert' ],
	attributeTypes : ["Agility", "Smarts", "Strength", "Spirit", "Vigor"],
	actions : {
		save : function() {
			var skillDescriptionsCreateController = this;
			return this.model.save().then(function(){
				skillDescriptionsCreateController.transitionToRoute('skilldescriptions.index');
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
			var skillDescriptionsCreateController = this;
			return this.model.save().then(function(){
				skillDescriptionsCreateController.transitionToRoute('skilldescriptions.index');
			}, function() {
			  // Couldn't save, do nothing about it.
			});
		}
	}
});