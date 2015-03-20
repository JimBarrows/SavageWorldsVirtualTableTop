import Ember from 'ember';

export default Ember.Controller.extend({
	plotPoints: 				[],
	startingAttributePoints: 	5,
	startingSkillPoints: 		15,
	startingMajorHindrances: 	1,
	startingMinorHindrances: 	2,
	attributePoints: 			5,
	skillPoints: 				15,
	majorHindrances: 			1,
	minorHindrances: 			2,
	actions: {
		save:function() {
			var controller = this;
			this.model.save().then(function( newCharacter) {
				Ember.get(controller, 'flashes').success('Success!', 2000);
				controller.transitionToRoute('character.edit', newCharacter);
			});
		},
		cancel: function() {
			this.model.destroyRecord();
			this.transitionToRoute('characters');
		},
		attributeIncremented: function() {
			this.set( "attributePoints", this.get("attributePoints") - 1);
		},
		attributeDecremented: function() {
			this.set( "attributePoints", this.get("attributePoints") + 1);
		}
	},
	hasAttributePoints: function() {
		if( this.get("attributePoints") < 0){
			return false;
		} else {
			return true;
		}
	}.property("attributePoints")
});
