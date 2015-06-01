Template.StoryList.helpers({
	stories: Stories.find({
		owner: Meteor.userId()
	}, {
		sort: {
			plotPointName: 1,
			name: 1
		}
	})
});

Template.StoryList.events({
	'click #addStory': function () {
		Router.go('story.add');
 	}
});