import Ember from 'ember';

export default Ember.Controller.extend({

	needs: "story",
	story: Ember.computed.alias("controllers.story"),
	actions: {
		save:function() {
			var controller = this;
			this.model.save().then(function( newChapter) {
				Ember.get(controller, 'flashes').success('Success!', 2000);
				var story = controller.get('story');
				story.get('chapters').addObject( newChapter);
				story.model.save().then( function( savedStory){
					controller.transitionToRoute('chapter.edit', newChapter);
				});
			});
		},
		cancel: function() {
			this.model.destroyRecord();
			this.transitionToRoute('story.edit');
		}
	}
});
