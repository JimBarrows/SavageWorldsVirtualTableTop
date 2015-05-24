Template.PlotPointList.helpers({
	plotPoints: PlotPoints.find({
		owner: Meteor.userId()
	})
});

Template.PlotPointList.events({
	'click #addPlotPoint': function() {
		Router.go('plotPoint.add');
	}
})