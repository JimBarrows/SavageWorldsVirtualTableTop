import Ember from 'ember';

export default Ember.Route.extend({	
	setupController: function(controller, model) {
		controller.set('standardRaces', this.store.find('standard-race'));
		controller.set('standardSkills', this.store.find('standard-skill-description'));
    	controller.set('model', model);
  	},
	model: function(params) {
	 
	 	return this.store.find('plot-point', params.plot_point_id);	 
	 }
});
