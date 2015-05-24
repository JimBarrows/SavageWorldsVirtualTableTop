Template.StoryForm.events({
  "submit #storyForm": function (event) {
    // This function is called when the new task form is submitted

    var id = event.target.id.value;
    var title = event.target.title.value;
    var description = $('#description').code();

    var storyRecord = {
      title: title,
      owner: Meteor.userId(),
      description: description
    };

    if( id) {
      Stories.update(id, storyRecord);
    } else {
      Stories.insert(storyRecord);
    }

    Router.go('story.list');
    // Prevent default form submit
    return false;
  }
}); 

Template.StoryForm.rendered = function() {
     $('#description').summernote({
        height: 200,   // set editable area's height
        //focus: true    // set focus editable area after Initialize summernote
     });
}
