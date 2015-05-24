Template.PlotPointItem.events({
	"click #deleteStory": function (event) {
		PlotPoints.remove(this._id);
	},
	"click #editStory": function( event) {
		Router.go('plotPoint.edit', {_id: this._id});
	},
	"click #viewStory": function( event) {
		Router.go('plotPoint.view', {_id: this._id});
	}
});