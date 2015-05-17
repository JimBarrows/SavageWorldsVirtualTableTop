import Ember from 'ember';

export default Ember.Controller.extend({
	needs: "plotPoint",
	plotPoint: Ember.computed.alias("controllers.plotPoint"),
	standards: [],
	selected: null,
	actions: {
		save: function() {
			var controller = this;
			controller.model.save().then(function( newModel){
				Ember.get(controller, 'flashes').success('Success!', 2000);
				var plotPoint = controller.get('plotPoint');
				plotPoint.get('archetypes').addObject(newModel);
				plotPoint.model.save().then( function( savedPlotPoint){
					controller.transitionToRoute('plot-point.edit');
					controller.set('selected', null);
				});
			});
		},
		cancel: function() {
			this.model.rollback();
			this.transitionToRoute('plot-point.edit');
			this.set('selected', null);
		},
		addStandard: function() {
			var selRace = this.get('selected');
			var model = this.get('model');
			model.set('name', selRace.get('name'));
			model.set('description', selRace.get('description'));

		}
	}
});