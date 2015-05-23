Template.Story.events({
	"click #deleteStory": function (event) {
		Stories.remove(this._id);
	}
});