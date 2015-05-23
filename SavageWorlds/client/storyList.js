Template.StoryList.helpers({
	stories: Stories.find({
		owner: Meteor.userId()
	})
});

Template.StoryList.events({
	'click #addStory': function () {
		Router.go('story.add');
 	}
});