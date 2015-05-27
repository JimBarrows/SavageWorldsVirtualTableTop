Template.SceneForm.events({
  "submit #sceneForm": function (event) {
  	var id = event.target.id.value;
  	var sceneRecord = {
  		title: event.target.title.value,
  		description: $('#description').code(),
  		story: this.story
  	};
  	if( id) {
  		Scenes.update(id, sceneRecord);
  	} else {
  		Scenes.insert( sceneRecord);
  	}
	Router.go('story.edit', {_id: this.story});
	// Prevent default form submit
	return false;
  }
});

Template.SceneForm.rendered = function() {
     $('#description').summernote({
        height: 200
     });
};