Template.RaceForm.events({
	"submit #raceForm": function( event, template) {
    	var plotPoint = this.plotPoint;
    	plotPoint.races = plotPoint.races ? plotPoint.races : [];
    	var raceRecord = {
    		index: plotPoint.races.length + 1,
    		name: event.target.name.value,
    		description: $("#description").code()
    	};
    	plotPoint.races.push(raceRecord);
    	PlotPoints.update( plotPoint._id, {
	  		$set: {
	  			races: plotPoint.races
	  		}
	  	});
        Router.go('plotPoint.edit', {_id: plotPoint._id});
        return false;
	}

});

Template.RaceForm.rendered = function() {
     $('#description').summernote({
        height: 200
     });
};