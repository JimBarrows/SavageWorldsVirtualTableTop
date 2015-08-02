import Ember from 'ember';

export default Ember.Controller.extend({
	needs: "plotPoint",
	plotPoint: Ember.computed.alias("controllers.plotPoint"),
	actions: {
		save: function() {
			var controller = this;
			controller.model.save().then(function( newModel){
				Ember.get(controller, 'flashes').success('Success!', 2000);
				var plotPoint = controller.get('plotPoint');
				plotPoint.get('races').addObject(newModel);
				plotPoint.model.save().then( function( savedPlotPoint){
					controller.transitionToRoute('plot-point.edit');
				});
			});
		},
		cancel: function() {
			this.model.rollback();
			this.transitionToRoute('plot-point.edit');
			this.set('selectedRace', null);
		}
	}
});