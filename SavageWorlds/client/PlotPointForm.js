Template.PlotPointForm.events({
	"submit #plotPointForm": function( event){
		var id = event.target.id.value;
		var plotPointRecord = {
			name: event.target.name.value,
			owner: Meteor.userId(),
			description: $('#description').code()
		};

		if( id) {
			PlotPoints.update( id, plotPointRecord);
		} else {
			PlotPoints.insert(plotPointRecord);
		}
		Router.go('plotPoint.list');
		return false;
	}
});

Template.PlotPointForm.rendered = function() {
     $('#description').summernote({
        height: 200
     });
};