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
	 	return newPlotPoint;
	 }
});
