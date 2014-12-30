App.ArmordescriptionsIndexController = Ember.Controller.extend({
	actions : {
		remove : function(armor) {
			armor.deleteRecord();
			return armor.save();
		}
	}
});

App.ArmordescriptionsCreateController = Ember.Controller.extend({
	needs : [ 'alert' ],	
	eras: ['Medieval', 'BlackPowder', 'Modern', 'Futuristic'],
	actions : {
		save : function() {
			var armorDescriptionsCreateController = this;
			return this.model.save().then(function(){
				armorDescriptionsCreateController.transitionToRoute('armordescriptions.index');
			}, function() {
			  // Couldn't save, do nothing about it.
			});
		}
	}
});

App.ArmordescriptionsEditController = Ember.Controller.extend({
	needs : [ 'alert' ],
	eras: ['Medieval', 'BlackPowder', 'Modern', 'Futuristic'],
	actions : {
		save : function() {
			var armorDescriptionsCreateController = this;
			return this.model.save().then(function(){
				armorDescriptionsCreateController.transitionToRoute('armordescriptions.index');
			}, function() {
			  // Couldn't save, do nothing about it.
			});
		}
	}
});