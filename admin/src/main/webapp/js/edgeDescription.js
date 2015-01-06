App.Skill = DS.Model.extend({

	description : DS.belongsTo('skillDescription', {
		async : true
	}),
	dice : DS.attr('string', {
		defaultValue : 'd4'
	}),
	bonus : DS.attr('number', {
		defaultValue : 0
	})

});

App.RankType = DS.Model.extend({
	name: DS.attr('string')
});

App.CharacterType = DS.Model.extend({
	name: DS.attr('string')
})

App.EdgeDescription = DS.Model.extend({
	edgeType : DS.belongsTo('edgeType', {
		async : true
	}),
	name : DS.attr('string'),
	minimumRank : DS.belongsTo('rankType', {
		async : true
	}),
	requiredType :  DS.belongsTo('characterType', {
		async : true
	}),
//	minimumSkills : DS.belongsTo('skill'),
//	requiredEdges : DS.belongsTo('edgeDescription'),
	version : DS.attr('number', {
		defaultValue : 0
	}),
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
	minimumRankList : function() {
		return this.store.find('rankType');
	}.property(),
	requiredTypeList : function() {
		return this.store.find('characterType');
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
	minimumRankList : function() {
		return this.store.find('rankType');
	}.property(),
	requiredTypeList : function() {
		return this.store.find('characterType');
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