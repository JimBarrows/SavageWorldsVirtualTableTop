import Ember from 'ember';

export default Ember.Route.extend({
	setupController: function(controller, model) {
		var plotPointList = this.store.find('plot-point');
		plotPointList.then(function(list){
			controller.set('selectedPlotPoint', list.get('firstObject'));
		})
		controller.set('plotPoints', this.store.find('plot-point'));
		controller.set('model', model);
	},
	model: function(params) {
		return this.store.find('story');
	}
});
