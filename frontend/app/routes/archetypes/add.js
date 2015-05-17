import Ember from 'ember';

export default Ember.Route.extend({
	setupController: function(controller, model) {
		controller.set('model', model);
		this.store.find('standard-archetype').then(function( standards){
			controller.set('standards', standards);
			controller.set('selected', standards.objectAtContent(0));	
		});
		
  	},
	model: function() {
		return this.store.createRecord('archetype');
	}
});
