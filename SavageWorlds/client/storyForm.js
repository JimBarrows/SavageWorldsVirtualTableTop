Template.StoryForm.events({
  "submit #storyForm": function (event) {
    // This function is called when the new task form is submitted

    var id = event.target.id.value;
    var title = event.target.title.value;
    var description = event.target.description.value;

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