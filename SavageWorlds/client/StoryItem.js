Template.StoryItem.events({
	"click #deleteStory": function (event) {
		Stories.remove(this._id);
	},
	"click #editStory": function( event) {
		Router.go('story.edit', {_id: this._id});
	},
	"click #viewStory": function( event) {
		Router.go('story.view', {_id: this._id});
	}
});