App.Skill = DS.Model.extend({

	edge : DS.belongsTo('edgeDescription', {
		async : true
	}),
	
	description : DS.belongsTo('skillDescription', {
		async : true
	}),
	dice : DS.belongsTo('diceType', {
		async : true
	}),
	bonus : DS.attr('number', {
		defaultValue : 0
	})

});

App.RankType = DS.Model.extend({
	name : DS.attr('string'),
	sequence : DS.attr('number')
});

App.CharacterType = DS.Model.extend({
	name : DS.attr('string'),
	sequence : DS.attr('number')
});

App.DiceType = DS.Model.extend({
	name : DS.attr('string'),
	sequence : DS.attr('number')
})

App.EdgeDescription = DS.Model.extend({
	edgeType : DS.belongsTo('edgeType', {
		async : true
	}),
	name : DS.attr('string'),
	minimumRank : DS.belongsTo('rankType', {
		async : true
	}),
	requiredType : DS.belongsTo('characterType', {
		async : true
	}),
	minimumSkills : DS.hasMany('skill', {
		async : true
	}),

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
	skillDescriptionList : function() {
		return this.store.find('skillDescription');
	}.property(),
	diceTypeList : function() {
		return this.store.find('diceType');
	}.property(),
	skillDescription : null,
	diceType : null,
	actions : {
		save : function() {
			var edgeDescriptionsEditController = this;
			return this.model.save().then(
					function() {
						edgeDescriptionsCreateController
								.transitionToRoute('edgedescriptions.index');
					}, function() {
						// Couldn't save, do nothing about it.
					});
		},
		addSkillDescription : function() {
			var controller = this;
			var model = this.get('model');
			var newSkill = this.store.createRecord('Skill', {
				edge : this.get('model'),
				description : this.get('skillDescription'),
				dice : this.get('diceType'),
				bonus : 0
			});
			newSkill.save().then(function(savedSkill) {
				model.get('minimumSkills').addObject(newSkill);
				model.save();
			});
		}
	}
});