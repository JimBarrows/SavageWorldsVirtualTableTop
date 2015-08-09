import Ember from 'ember';

export default Ember.Controller.extend({

	standardRaces:[],
	standardSkills: [],
	standardEdges: [],
	standardHindrances: [],
	standardGear: [],
	standardPlaces: [],
	standardArchetypes: [],
	standardCharacters: [],
	standardExtras: [],
	standardPowers: [],
	standardBeasts: [],

	actions: {
		save:function() {			
			var controller = this;
			var model = this.get('model');
			model.save().then(function( plotPoint) {
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
				controller.model.get('races').addRecord(newRecord);
				controller.model.save();
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
				controller.model.get('skills').addRecord(newRecord);
				controller.model.save();
			});
		},
		addEdge: function( edge, ops) {
			var newRecord = this.store.createRecord('edge',{
				name: edge.get('name'),
				description: edge.get('description')
			});
			var controller = this;
			newRecord.save().then(function(res){
				controller.model.get('edges').addRecord(newRecord);
				controller.model.save();
			});
		},
		addHindrance: function( hindrance, ops) {
			var newRecord = this.store.createRecord('hindrance',{
				name: hindrance.get('name'),
				severity: hindrance.get('severity'),
				description: hindrance.get('description')
			});
			var controller = this;
			newRecord.save().then(function(res){
				controller.model.get('hindrances').addRecord(newRecord);
				controller.model.save();
			});
		},
		addGear: function( gear, ops) {
			var newRecord = this.store.createRecord('gear',{
				name: gear.get('name'),
				description: gear.get('description'),
				era: gear.get('era'),
	 			weight: gear.get('weight'),
	 			cost: gear.get('cost'),
	 			subType: gear.get('subType'),
	 			notes: gear.get('notes')
			});
			var controller = this;
			newRecord.save().then(function(res){
				controller.model.get('gears').addRecord(newRecord);
				controller.model.save();
			});
		},
		
		addPower: function( power, ops) {
			var controller = this;
			var newRecord = this.store.createRecord('power',{
				name: power.get('name'),
				description: power.get('description')
			});
			newRecord.save().then( function( res){
				controller.model.get('powers').addRecord(newRecord);
				controller.model.save();				
			});
			
		}
	}
});
