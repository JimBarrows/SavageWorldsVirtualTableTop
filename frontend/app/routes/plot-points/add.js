import Ember from 'ember';

export default Ember.Route.extend({
	
	 model: function(params) {	 	
	 	var store = this.store;
	 	var newCharacter = store.createRecord('plot-point');
	 	store.find('standard-skill-description').then(function(standardSkills){
	 		standardSkills.forEach(function( stdSkill){
	 			newCharacter.get('skills').addObject(store.createRecord('skill-description',{
	 				name: stdSkill.get('name'),
	 				description: stdSkill.get('description'),
	 				attribute: stdSkill.get('attribute')
	 			}));
	 		});	
	 	});
	 	
	 	store.find('standard-hindrance').then(function(standardHindrances){
	 		standardHindrances.forEach( function( hindrance){
	 			newCharacter.get('hindrances').addObject(store.createRecord('hindrance', {
	 				name: hindrance.get('name'),
	 				description: hindrance.get('description'),
	 				severity: hindrance.get('severity')
	 			}))
	 		});
	 	});
	 	
	 	return newCharacter;
	 }
});
