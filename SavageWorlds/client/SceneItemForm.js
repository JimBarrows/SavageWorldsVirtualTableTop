Template.SceneItemForm.rendered = function() {
     $('#sceneDescription').summernote({
        height: 200,   // set editable area's height
        //focus: true    // set focus editable area after Initialize summernote
     });
};

Template.SceneItemForm.helpers({
  sceneTitle: "New Scene",
  sceneDescription: "New Description"
});

Template.SceneItemForm.events({
	"click #saveScene": function( event, template) {
		var scene = {
			title: template.find("#sceneTitle").value,
			description: template.$("#sceneDescription").code(),
			story: this._id
		}
		Scenes.insert( scene);
		Session.set("showSceneForm", false);
		return false;
	}
});