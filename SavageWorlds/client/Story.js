Template.Story.events({
	"click #done": function (event) {
		Router.go('story.list');
	}
});

Template.Story.helpers({
	scenes: function() {
   		return Scenes.find({
			story: this._id
		});
	}
});