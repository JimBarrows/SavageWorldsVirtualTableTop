import Ember from 'ember';

export default Ember.Controller.extend({
	selectedPlotPoint: {},
	plotPoints: [],
	actions: {
		add: function() {
			var spp = this.get('selectedPlotPoint');
			this.transitionToRoute('characters.add');	
		},
		remove: function( newRec) {
			newRec.destroyRecord();
		}
	}
});
