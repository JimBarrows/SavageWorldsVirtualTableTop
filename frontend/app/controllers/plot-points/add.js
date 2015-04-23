import Ember from 'ember';

export default Ember.Controller.extend({
	actions: {
		save:function() {
			var controller = this;
			// this.model.get('races').forEach( function( race) {
			// 	race.save();
			// });
			this.model.save().then(function( newPlotPoint) {
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
