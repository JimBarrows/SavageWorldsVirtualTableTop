import Ember from 'ember';

export default Ember.Route.extend({
	setupController: function(controller, model) {
		controller.set('model', model);
		this.store.find('standard-race').then(function( standardRaces){
			controller.set('standardRaces', standardRaces);
			controller.set('selectedRace', standardRaces.objectAtContent(0));	
		});
		
  	},
	model: function() {
		return this.store.createRecord('race');
	}
});
