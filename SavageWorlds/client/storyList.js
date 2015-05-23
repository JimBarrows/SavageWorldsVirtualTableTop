Template.StoryList.helpers({
	stories: Stories.find({})
});

Template.StoryList.events({
	'click #addStory': function () {
		Router.go('story.add');
 	}
});