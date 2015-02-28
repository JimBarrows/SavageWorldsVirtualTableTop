import Ember from 'ember';

export default Ember.Controller.extend({

	standardRaces:[],

	actions: {
		save:function() {			
			var controller = this;
			this.model.save().then(function() {
				Ember.get(controller, 'flashes').success('Success!', 2000);				
			});
		},
		cancel: function() {
			console.log("cancel");
			this.model.rollback();
			this.transitionToRoute('index');
		},
		addRace: function( race, ops) {
			var newRace = this.store.createRecord('race',{
				name: race.get('name'),
				description: race.get('description'),
			});
			var controller = this;
			newRace.save(). then(function(res){
				controller.model.get('races').addRecord(newRace);
				controller.model.save();
			});
			
		}
	}
});
