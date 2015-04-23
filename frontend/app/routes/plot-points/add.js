import Ember from 'ember';

export default Ember.Route.extend({
	
	model: function(params) {	 	
		var store = this.store;
		var newCharacter = store.createRecord('plot-point');
	 	store.find('standard-race').then(function(list){
	 		list.forEach(function( item){
	 			var newRace = store.createRecord('race',{
	 				name: item.get('name'),
	 				description: item.get('description')
	 			});
	 			newRace.save().then(function( savedRace){
	 				newCharacter.get('races').addObject(savedRace);	
	 			});
	 		});	
	 	});
	 	return newCharacter;
	 }
});
