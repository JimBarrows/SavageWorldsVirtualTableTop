Template.StoryForm.events({
  "submit #storyForm": function (event) {
    // This function is called when the new task form is submitted

    var title = event.target.title.value;
console.log("title: " + title);
    Stories.insert({
      title: title
    })

    // Clear form
    event.target.title.value = "";

    Router.go('/');
    // Prevent default form submit
    return false;
  }
}); 