import Ember from 'ember';

export default Ember.Route.extend({	
	setupController: function(controller, model) {
		controller.set('standardRaces', this.store.find('standard-race'));
		controller.set('standardSkills', this.store.find('standard-skill-description'));
		controller.set('standardEdges', this.store.find('standard-edge'));
		controller.set('standardHindrances', this.store.find('standard-hindrance'));
		controller.set('standardGear', this.store.find('standard-gear'));
		controller.set('standardPlaces', this.store.find('standard-place'));
		controller.set('standardArchetypes', this.store.find('standard-archetype'));
		controller.set('standardCharacters', this.store.find('standard-character'));
		controller.set('standardExtras', this.store.find('standard-extra'));
		controller.set('standardPowers', this.store.find('standard-power'));
		controller.set('standardBeasts', this.store.find('standard-beast'));
    	controller.set('model', model);
  	},
	model: function(params) {
	 	return this.modelFor("plot-point");
	 }
});
