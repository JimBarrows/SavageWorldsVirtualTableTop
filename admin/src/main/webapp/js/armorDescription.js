App.ArmorDescription = DS.Model.extend({
	version : DS.attr('number', {
		defaultValue : 0
	}),
	name : DS.attr('string'),
	armor : DS.attr('number'),
	vsBullets : DS.attr('number'),
	weight : DS.attr('number'),
	cost : DS.attr('number'),
	notes : DS.attr('string'),
	era : DS.attr('string')
});

App.ArmordescriptionsIndexRoute = Ember.Route.extend({
	model : function() {
		return this.store.find('ArmorDescription');
	}
});

App.ArmordescriptionsCreateRoute = Ember.Route.extend({
	model : function() {
		return this.store.createRecord('ArmorDescription');
	}
});

App.ArmordescriptionsEditRoute = Ember.Route.extend({
	model : function(params) {
		return this.store.find('armorDescription', params.armor_description_id)
	}
});

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
	eras : [ 'Medieval', 'BlackPowder', 'Modern', 'Futuristic' ],
	actions : {
		save : function() {
			var armorDescriptionsCreateController = this;
			return this.model.save().then(
					function() {
						armorDescriptionsCreateController
								.transitionToRoute('armordescriptions.index');
					}, function() {
						// Couldn't save, do nothing about it.
					});
		}
	}
});

App.ArmordescriptionsEditController = Ember.Controller.extend({
	needs : [ 'alert' ],
	eras : [ 'Medieval', 'BlackPowder', 'Modern', 'Futuristic' ],
	actions : {
		save : function() {
			var armorDescriptionsCreateController = this;
			return this.model.save().then(
					function() {
						armorDescriptionsCreateController
								.transitionToRoute('armordescriptions.index');
					}, function() {
						// Couldn't save, do nothing about it.
					});
		}
	}
});