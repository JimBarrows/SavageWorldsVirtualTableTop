App.EdgeType = DS.Model.extend({
	version   : DS.attr('number', {defaultValue: 0}),
	name      : DS.attr('string'),
	description : DS.attr('string')
});

App.EdgetypesIndexRoute = Ember.Route.extend({
	model : function() {
		return this.store.find('EdgeType');
	}
});

App.EdgetypesCreateRoute = Ember.Route.extend({
	model : function() {
		return this.store.createRecord('EdgeType');
	}
});

App.EdgetypesEditRoute = Ember.Route.extend({
	model : function(params) {
		return this.store.find('edgeType', params.edge_type_id)
	}	
});

App.EdgetypesIndexController = Ember.Controller.extend({
	actions : {
		remove : function(record) {
			record.deleteRecord();
			return record.save();
		}
	}
});

App.EdgetypesCreateController = Ember.Controller.extend({
	needs : [ 'alert' ],	
	actions : {
		save : function() {
			var edgeTypesCreateController = this;
			return this.model.save().then(function(){
				edgeTypesCreateController.transitionToRoute('edgetypes.index');
			}, function() {
			  // Couldn't save, do nothing about it.
			});
		}
	}
});

App.EdgetypesEditController = Ember.Controller.extend({
	needs : [ 'alert' ],
	actions : {
		save : function() {
			var edgeTypesCreateController = this;
			return this.model.save().then(function(){
				edgeTypesCreateController.transitionToRoute('edgetypes.index');
			}, function() {
			  // Couldn't save, do nothing about it.
			});
		}
	}
});