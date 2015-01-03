App.EdgeDescription = DS.Model.extend({
	version : DS.attr('number', {
		defaultValue : 0
	}),
	name : DS.attr('string'),
	edgeType : DS.belongsTo('edgeType', {
		async : true
	})
});

App.EdgedescriptionsIndexRoute = Ember.Route.extend({
	model : function() {
		return this.store.find('EdgeDescription');
	}
});

App.EdgedescriptionsCreateRoute = Ember.Route.extend({
	model : function() {
		return this.store.createRecord('EdgeDescription');
	}
});

App.EdgedescriptionsEditRoute = Ember.Route.extend({
	model : function(params) {
		return this.store.find('edgeDescription', params.edge_description_id)
	}
});

App.EdgedescriptionsIndexController = Ember.Controller.extend({
	relation : Ember.computed.alias('content.relation.content'),
	actions : {
		remove : function(record) {
			record.deleteRecord();
			return record.save();
		}
	}
});

App.EdgedescriptionsCreateController = Ember.Controller.extend({
	needs : [ 'alert' ],
	edgeTypeList : function() {
		return this.store.find('edgeType');
	}.property(),
	actions : {
		save : function() {
			var edgeDescriptionsCreateController = this;
			return this.model.save().then(
					function() {
						edgeDescriptionsCreateController
								.transitionToRoute('edgedescriptions.index');
					}, function() {
						// Couldn't save, do nothing about it.
					});
		}
	}
});

App.EdgedescriptionsEditController = Ember.Controller.extend({
	needs : [ 'alert' ],
	edgeTypeList : function() {
		return this.store.find('edgeType');
	}.property(),
	actions : {
		save : function() {
			var edgeDescriptionsCreateController = this;
			return this.model.save().then(
					function() {
						edgeDescriptionsCreateController
								.transitionToRoute('edgedescriptions.index');
					}, function() {
						// Couldn't save, do nothing about it.
					});
		}
	}
});