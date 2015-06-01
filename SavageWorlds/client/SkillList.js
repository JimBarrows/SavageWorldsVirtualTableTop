Template.SkillList.helpers({
	skills: Skills.find({
	}, {
		sort: {
			name: 1,
			attribute: 1
		}
	})
});

Template.SkillList.events({
	'click #add': function() {
		Router.go('skill.add');
	},
	'click #editSkill': function() {
		Router.go('skill.edit', {_id: this._id});
	}
})