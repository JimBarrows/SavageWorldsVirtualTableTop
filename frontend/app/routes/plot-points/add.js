import Ember from 'ember';

export default Ember.Route.extend({
	
	model: function(params) {	 	
		var store = this.store;
		var newPlotPoint = store.createRecord('plot-point');
	 	store.find('standard-skill-description').then(function(list){
	 		list.forEach(function( item){
	 			var newRec = store.createRecord('skill-description',{
	 				name: item.get('name'),
	 				description: item.get('description'),
	 				attribute: item.get('attribute')
	 			});
	 			newRec.save().then(function( savedRec){
	 				newPlotPoint.get('skillDescriptions').addObject(savedRec);	
	 			});
	 		});	
	 	});
	 	store.find('standard-hindrance').then(function(list){
	 		list.forEach(function( item){
	 			var newRec = store.createRecord('hindrance',{
	 				name: item.get('name'),
	 				description: item.get('description'),
	 				severity: item.get('severity')
	 			});
	 			newRec.save().then(function( savedRec){
	 				newPlotPoint.get('hindrances').addObject(savedRec);
	 			});
	 		});	
	 	});
	 	store.find('standard-edge').then(function(list){
	 		list.forEach(function( item){
	 			var newRec = store.createRecord('edge',{
	 				name: item.get('name'),
	 				description: item.get('description')
	 			});
	 			newRec.save().then(function( savedRec){
	 				newPlotPoint.get('edges').addObject(savedRec);
	 			});
	 		});	
	 	});
	 	store.find('standard-power').then(function(list){
	 		list.forEach(function( item){
	 			var newRec = store.createRecord('power',{
	 				name: item.get('name'),
	 				description: item.get('description'),
	 				powerPoints: item.get('powerPoints'),
	 				rank: item.get('rank'),
	 				range: item.get('range'),
	 				duration: item.get('range')
	 			});
	 			newRec.save().then(function( savedRec){
	 				newPlotPoint.get('powers').addObject(savedRec);
	 			});
	 		});	
	 	});
	 	store.find('standard-gear').then(function(list){
	 		list.forEach(function( item){
	 			var newRec = store.createRecord('gear',{
	 				name: item.get('name'),
	 				description: item.get('description'),
	 				era: item.get('era'),
	 				weight: item.get('weight'),
	 				cost: item.get('cost'),
	 				subType: item.get('subType'),
	 				notes: item.get('notes')
	 			});
	 			newRec.save().then(function( savedRec){
	 				newPlotPoint.get('gears').addObject(savedRec);
	 			});
	 		});	
	 	});
	 	return newPlotPoint;
	 }
});
