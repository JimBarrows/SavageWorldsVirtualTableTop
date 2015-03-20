import Ember from 'ember';

export default Ember.Route.extend({
	setupController: function( controller, model){
		model.set('plotPoint', this.modelFor('plot-point'));
		controller.set('model', model);
	},
	model: function(params) {	 	
		var newCharacter = this.store.createRecord('character');
	 	return newCharacter;
	}
});
