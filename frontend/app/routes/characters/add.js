import Ember from 'ember';

export default Ember.Route.extend({
	setupController: function( controller, model){
		var plotPoint = model.get('plotPoint');
		controller.set('startingAttributePoints', plotPoint.get('startingAttributePoints'));
		controller.set('attributePoints', plotPoint.get('startingAttributePoints'));
		controller.set('startingSkillPoints', plotPoint.get('startingSkillPoints'));
		controller.set('skillPoints', plotPoint.get('startingSkillPoints'));
		controller.set('startingMajorHindrances', plotPoint.get('startingMajorHindrances'));
		controller.set('majorHindrances', plotPoint.get('startingMajorHindrances'));
		controller.set('startingMinorHindrances', plotPoint.get('startingMinorHindrances'));
		controller.set('minorHindrances', plotPoint.get('startingMinorHindrances'));
		controller.set('startingCash', plotPoint.get('startingCash'));
		controller.set('cash', plotPoint.get('startingCash'));
		controller.set('availableHindrances', plotPoint.get('hindrances'));
		controller.set('availableRaces', plotPoint.get('races'));
		controller.set('availableEdges', plotPoint.get('edges'));
		controller.set('availableGears', plotPoint.get('gears'));
		controller.set('availablePowers', plotPoint.get('powers'));
		controller.set('model', model);
	},
	model: function(params) {	 	
		var route = this;
		var newCharacter = this.store.createRecord('character');
		var plotPointController = this.controllerFor('characters.index');
		var selectedPlotPoint = plotPointController.get("selectedPlotPoint");
		newCharacter.set('plotPoint', selectedPlotPoint);
		selectedPlotPoint.get('skillDescriptions').forEach(function( skillDescription){
			var newSkill = route.store.createRecord('skill',{
				description: skillDescription
			});
			newSkill.save().then(function(savedSkill){
				newCharacter.get('skills').addRecord(savedSkill);
			});
			
		});
		newCharacter.set('cash', selectedPlotPoint.get('startingCash'));
	 	return newCharacter;
	}
});
