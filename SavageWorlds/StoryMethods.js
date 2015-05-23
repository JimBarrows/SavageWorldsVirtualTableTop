Meteor.methods({
	addStory: function (story) {
		Stories.insert(story);
	}
});