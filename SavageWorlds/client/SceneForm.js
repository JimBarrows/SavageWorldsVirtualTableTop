Template.SceneForm.helpers({
	showSelectionForm: function() {
		return Session.get('showSelectionForm');
	},
	characterName: ""
	
});

Template.SceneForm.events({
 	"submit #sceneForm": function (event) {
		var id = event.target.id.value;
		var sceneRecord = {
			title: event.target.title.value,
			description: $('#description').code(),
			story: this.story,
			dramatisPersonae: this.dramatisPersonae
		};
		if( id) {
			Scenes.update(id, sceneRecord);
		} else {
			Scenes.insert( sceneRecord);
		}

		Router.go('story.edit', {_id: this.story});
		// Prevent default form submit
		return false;
	},
	"click #addCharacter": function(event, template){
	  	Session.set('showSelectionForm', true);
	  	return false;
  	},
  	"click #addPersonaeDramatis": function( event, template){
  		var dramatisPersonae =  this.dramatisPersonae ? this.dramatisPersonae : [];
  		var characterName = template.$('#characterName');
  		var index = dramatisPersonae.length + 1;
	  	dramatisPersonae.push({
	  		name: characterName.val(),
	  		index: index
	  	});
	  	Scenes.update( this._id, {
	  		$set: {
	  			dramatisPersonae: dramatisPersonae
	  		}
	  	});
  		Session.set('showSelectionForm', false);
  		return false;
  	},
  	"click #deleteCharacter": function( event, template) {
  		var dramatisPersonae = template.data.dramatisPersonae;
  		var removeThis = this.index;
  		Scenes.update(template.data._id,{
  			$set:{
  				dramatisPersonae: dramatisPersonae.filter( function( person){
  					return person.index != removeThis;
  				})
  			}
  		});
  		return false;
  	}
});

Template.SceneForm.rendered = function() {

     $('#description').summernote({
        height: 200
     });
};