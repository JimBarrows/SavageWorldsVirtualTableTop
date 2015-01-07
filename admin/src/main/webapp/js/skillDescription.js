/**
 * 
 */
App.SkillDescription = DS.Model.extend({
	version : DS.attr('number', {
		defaultValue : 0
	}),
	name : DS.attr('string'),
	attribute : DS.attr('string'),
	description : DS.attr('string')
});

App.SkilldescriptionsIndexRoute = Ember.Route.extend({
	model : function() {
		return this.store.find('SkillDescription');
	}
});

App.SkilldescriptionsCreateRoute = Ember.Route.extend({
	model : function() {
		return this.store.createRecord('SkillDescription');
	}
});

App.SkilldescriptionsEditRoute = Ember.Route.extend({
	model : function(params) {
		return this.store.find('skillDescription', params.skill_description_id)
	}
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
	attributeTypes : [ "Agility", "Smarts", "Strength", "Spirit", "Vigor" ],
	actions : {
		save : function() {
			var skillDescriptionsCreateController = this;
			return this.model.save().then(
					function() {
						skillDescriptionsCreateController
								.transitionToRoute('skilldescriptions.index');
					}, function() {
						// Couldn't save, do nothing about it.
					});
		}
	}
});

App.SkilldescriptionsEditController = Ember.Controller.extend({
	needs : [ 'alert' ],
	attributeTypes : [ "Agility", "Smarts", "Strength", "Spirit", "Vigor" ],
	actions : {
		save : function() {
			var skillDescriptionsCreateController = this;
			return this.model.save().then(
					function() {
						skillDescriptionsCreateController
								.transitionToRoute('skilldescriptions.index');
					}, function() {
						// Couldn't save, do nothing about it.
					});
		}
	}
});