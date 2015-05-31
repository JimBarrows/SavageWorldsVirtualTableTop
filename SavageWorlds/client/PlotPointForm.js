Template.PlotPointForm.events({
	"submit #plotPointForm": function( event){
		var id = event.target.id.value;
		var plotPointRecord = {
			name: event.target.name.value,
			owner: Meteor.userId(),
			description: $('#description').code(),
			races: this.races
		};

		if( id) {
			PlotPoints.update( id, plotPointRecord);
		} else {
			PlotPoints.insert(plotPointRecord);
		}
		Router.go('plotPoint.list');
		return false;
	},
	"click #addRace": function( event, template) {
		Router.go('race.add', { 
			plotPointId: this._id
		});
		return false;
	},
	"click #deleteRace": function( event, template) {
		var raceToBeDeleted = this.index;
        var races = template.data.races.filter(function( race) {
        	return raceToBeDeleted != race.index;
        });
        PlotPoints.update( template.data._id, {
	  		$set: {
	  			races: races
	  		}
	  	});
	  	return false;
    }
});

Template.PlotPointForm.rendered = function() {
     $('#description').summernote({
        height: 200
     });
};