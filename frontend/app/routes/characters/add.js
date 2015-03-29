import Ember from 'ember';

export default Ember.Route.extend({
	setupController: function( controller, model){
		controller.set('model', model);
	},
	model: function(params) {	 	
		var newCharacter = this.store.createRecord('character');
		var plotPoint = this.modelFor('plot-point');
		var skills = [];
		var route = this;
		plotPoint.get('skills').forEach(function( skill){
			newCharacter.get('skills').addRecord(route.store.createRecord('skill', {
				description: skill
			}));
		});
		newCharacter.set('plotPoint', plotPoint);
	 	return newCharacter;
	}
});
