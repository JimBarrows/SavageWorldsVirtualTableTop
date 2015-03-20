import Ember from 'ember';

export default Ember.Controller.extend({
	selectedPlotPoint: null,
	actions: {
		add: function() {
			var selectedPlotPoint = this.get('selectedPlotPoint');
			if( selectedPlotPoint === null) {
				this.transitionToRoute('characters.add', this.get('plotPoints').objectAtContent(0));
			} else {
				this.transitionToRoute('characters.add', this.get('selectedPlotPoint'));	
			}
			
		},
		remove: function( newRec) {
			newRec.destroyRecord();
		}
	}
});
