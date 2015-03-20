import Ember from 'ember';

export default Ember.Route.extend({
	setupController: function(controller, model) {
		controller.set('plotPoints', this.store.find('plot-point'));
		controller.set('model', model);
	},
	model: function(params) {
		return this.store.find('character');
	}
});
