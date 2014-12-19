/**
 * 
 */

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