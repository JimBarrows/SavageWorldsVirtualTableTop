Template.Story.events({
	"click #deleteStory": function (event) {
		Stories.remove(this._id);
	},
	"click #editStory": function( event) {
		Router.go('story.edit', {_id: this._id});
	}
});