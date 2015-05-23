Template.Story.events({
	"click #done": function (event) {
		Router.go('story.list');
	}
});