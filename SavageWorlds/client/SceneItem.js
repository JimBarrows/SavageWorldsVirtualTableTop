Template.SceneItem.events({
	"click #deleteScene": function (event) {
		console.log("this._id: " + this._id);
		Scenes.remove(this._id);
	},
});