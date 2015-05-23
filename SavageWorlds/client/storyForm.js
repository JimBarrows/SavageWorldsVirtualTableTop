Template.StoryForm.events({
  "submit #storyForm": function (event) {
    // This function is called when the new task form is submitted

    var title = event.target.title.value;
    var id = event.target.id.value;
    if( id) {
      Stories.update(id, {
        title: title
      });
      
    } else {
      Stories.insert({
        title: title
      });
    }

    Router.go('story.list');
    // Prevent default form submit
    return false;
  }
}); 