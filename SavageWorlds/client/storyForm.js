Template.StoryForm.helpers({

  plotPointList: function() {
    return PlotPoints.find({
      owner: Meteor.userId()
    })
  },
  plotPointSelected: function(plotPointId) {
    if (plotPointId == this._id)
        { return "selected";}
    else 
        { return "";}
  },
  showSceneForm: function () {
    return Session.get("showSceneForm");
  },
  scenes: function() {
    return Scenes.find({
      story: this._id
    });
  }
});

Template.StoryForm.events({
  "submit #storyForm": function (event) {

    var id = event.target.id.value;
    var target = event.target;
    var plotPointId = target.plotPointId;

    var storyRecord = {
      title: event.target.title.value,
      owner: Meteor.userId(),
      description: $('#description').code(),
      plotPointId: event.target.plotPointId.value,
      plotPointName: event.target.plotPointId.options[event.target.plotPointId.selectedIndex].text
    };

    if( id) {
      Stories.update(id, storyRecord);
    } else {
      Stories.insert(storyRecord);
    }

    Router.go('story.list');
    // Prevent default form submit
    return false;
  },
  "click #addScene": function( event) {
    Session.set("showSceneForm", true);
    return false;
  }
}); 

Template.StoryForm.rendered = function() {
     $('#description').summernote({
        height: 200,   // set editable area's height
        //focus: true    // set focus editable area after Initialize summernote
     });
};
