import Ember from 'ember';

export default Ember.Controller.extend({
	selectedPlotPoint: null,
	actions: {
		add: function() {
			this.transitionToRoute('characters.add', this.get('selectedPlotPoint'));
		},
		remove: function( newRec) {
			newRec.destroyRecord();
		}
	}
});
