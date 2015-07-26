import Ember from 'ember';

export default Ember.Route.extend({
	
	model: function(params) {	 	
		var store = this.store;
		var newCharacter = store.createRecord('plot-point');
	 	store.find('standard-skill-description').then(function(list){
	 		list.forEach(function( item){
	 			var newRec = store.createRecord('skill-description',{
	 				name: item.get('name'),
	 				description: item.get('description'),
	 				attribute: item.get('attribute')
	 			});
	 			newRec.save().then(function( savedRec){
	 				newCharacter.get('skillDescriptions').addObject(savedRec);	
	 			});
	 		});	
	 	});
	 	return newCharacter;
	 }
});
