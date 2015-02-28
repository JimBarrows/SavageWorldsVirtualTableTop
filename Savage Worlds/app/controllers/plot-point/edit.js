import Ember from 'ember';

export default Ember.Controller.extend({

	standardRaces:[],
	standardSkills: [],

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
			newRace.save().then(function(res){
				controller.model.get('races').then(function(races){
					races.addRecord(newRace);
					controller.model.save();
				});
			});
			
		},
		addSkill: function( skillDescription, ops) {
			var newSkill = this.store.createRecord('skill-description',{
				name: skillDescription.get('name'),
				description: skillDescription.get('description'),
				attribute: skillDescription.get('attribute')
			});
			var controller = this;
			newSkill.save().then(function(res){
				controller.model.get('skills').then(function( skills){
					skills.addRecord(newSkill);
					controller.model.save();
				});
			});
		}
	}
});
