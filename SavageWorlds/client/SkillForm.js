Template.SkillForm.helpers({

  attributeList: ["Agility", "Smarts", "Strength", "Spirit", "Vigor"]
});

Template.SkillForm.events({
 	"submit #skill-form": function (event) {
		var id = event.target.id.value;
		var skillRecord = {
			name: event.target.name.value,
			attribute: event.target.attribute.value,
			description: $('#description').code()
		};
		if( id) {
			Skills.update(id, skillRecord);
		} else {
			Skills.insert( skillRecord);
		}

		Router.go('skill.list');

		return false;
	}
});
Template.SkillForm.rendered = function() {
     $('#description').summernote({
        height: 200
     });
};