import Ember from 'ember';

export default Ember.Controller.extend({
	needs: "plotPoint",
	plotPoint: Ember.computed.alias("controllers.plotPoint"),
	newCost: 0,
	newDescription: '',
	totalCost: -1,
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
		},
		addAbility: function() {
			var controller = this;
			var newRacialAbility = this.store.createRecord('RacialAbility', {
				cost: controller.get('newCost'),
				description: controller.get('newDescription')
			});
			newRacialAbility.save().then(function( savedRacialAbility){
				controller.model.get('racialAbilities').addObject(savedRacialAbility);
				controller.incrementProperty( 'totalCost', savedRacialAbility.get('cost'));
				controller.model.save();
			});

			return false;
		},
		removeAbility: function( ability) {
			var controller = this;
			ability.destroyRecord().then( function(){
				controller.decrementProperty( 'totalCost', ability.get('cost'));
			});
		}
	}
});