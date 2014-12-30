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

App.SkilldescriptionsEditRoute = Ember.Route.extend({
	model : function(params) {
		return this.store.find('skillDescription', params.skill_description_id)
	}	
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