Template.SceneItem.events({
	"click #deleteScene": function (event) {
		Scenes.remove(this._id);
	},
});