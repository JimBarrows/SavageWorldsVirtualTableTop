import Ember from 'ember';

export default Ember.Controller.extend({

	standardRaces:[],
	standardSkills: [],
	standardEdges: [],
	standardHindrances: [],
	standardGear: [],
	standardPlaces: [],
	standardArchetypes: [],

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
				description: gear.get('description')
			});
			var controller = this;
			newRecord.save().then(function(res){
				controller.model.get('gear').addRecord(newRecord);
				controller.model.save();
			});
		},
		addPlace: function( place, ops) {
			var controller = this;
			var newRecord = this.store.createRecord('place',{
				name: place.get('name'),
				description: place.get('description')
			});
			newRecord.save().then( function( res){
				controller.model.get('places').addRecord(newRecord);
				controller.model.save();
			});
		},
		addArchetype: function( archetype, ops) {
			var controller = this;
			var newRecord = this.store.createRecord('archetype',{
				name: archetype.get('name'),
				description: archetype.get('description')
			});
			newRecord.save().then( function( res){
				controller.model.get('archetypes').addRecord(newRecord);
				controller.model.save();				
			});
			
		}
	}
});
