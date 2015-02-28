import Ember from 'ember';

export default Ember.Controller.extend({

	standardRaces:[],
	standardSkills: [],
	standardEdges: [],

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
			var newRecord = this.store.createRecord('race',{
				name: race.get('name'),
				description: race.get('description'),
			});
			var controller = this;
			newRecord.save().then(function(res){
				controller.model.get('races').then(function(races){
					races.addRecord(newRecord);
					controller.model.save();
				});
			});
			
		},
		addSkill: function( skillDescription, ops) {
			var newRecord = this.store.createRecord('skill-description',{
				name: skillDescription.get('name'),
				description: skillDescription.get('description'),
				attribute: skillDescription.get('attribute')
			});
			var controller = this;
			newRecord.save().then(function(res){
				controller.model.get('skills').then(function( skills){
					skills.addRecord(newRecord);
					controller.model.save();
				});
			});
		},
		addEdge: function( edge, ops) {
			var newRecord = this.store.createRecord('edge',{
				name: edge.get('name'),
				description: edge.get('description')
			});
			var controller = this;
			newRecord.save().then(function(res){
				controller.model.get('edges').then(function( edges){
					edges.addRecord(newRecord);
					controller.model.save();
				});
			});
		},
		addHindrance: function( hindrance, ops) {
			var newRecord = this.store.createRecord('hindrance',{
				name: hindrance.get('name'),
				description: hindrance.get('description')
			});
			var controller = this;
			newRecord.save().then(function(res){
				controller.model.get('hindrances').then(function( hindrances){
					hindrances.addRecord(newRecord);
					controller.model.save();
				});
			});
		}
	}
});
