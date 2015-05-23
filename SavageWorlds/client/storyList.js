Template.storyList.helpers({
	stories: Stories.find({})
});

Template.storyList.events({
	'click #addStory': function () {
		Router.go('/storyForm');
 	}
});