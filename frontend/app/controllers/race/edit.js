import Ember from 'ember';

export default Ember.Controller.extend({

	actions: {
		save:function() {			
			var controller = this;
			this.model.save().then(function() {
				Ember.get(controller, 'flashes').success('Success!', 2000);				
				controller.transitionToRoute('plot-point.edit');
			});
		},
		cancel: function() {
			this.model.rollback();
			this.transitionToRoute('plot-point.edit');
		},
		addAbility: function() {
			var controller = this;
			var newRacialAbility = this.store.createRecord('RacialAbility', {
				cost: controller.get('newCost'),
				description: controller.get('newDescription')
			});
			newRacialAbility.save().then(function( savedRacialAbility){
				controller.model.get('racialAbilities').addObject(savedRacialAbility);
				controller.model.save();
			});
			return false;
		},
		removeAbility: function( ability) {
			ability.destroyRecord();

		}
	}
});
