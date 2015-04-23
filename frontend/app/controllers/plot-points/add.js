import Ember from 'ember';

export default Ember.Controller.extend({
	races:[],

	actions: {
		save:function() {
			var controller = this;
			var model = this.get('model');
			model.save().then(function( newPlotPoint) {
				Ember.get(controller, 'flashes').success('Success!', 2000);
				controller.transitionToRoute('plot-point.edit', newPlotPoint);
			});
		},
		cancel: function() {
			this.model.destroyRecord();
			this.transitionToRoute('index');
		}
	}
});
