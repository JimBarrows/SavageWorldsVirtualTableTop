Template.PlotPointList.helpers({
	plotPoints: PlotPoints.find({
		owner: Meteor.userId()
	}, {
		sort: {
			name: 1
		}
	})
});

Template.PlotPointList.events({
	'click #addPlotPoint': function() {
		Router.go('plotPoint.add');
	}
})