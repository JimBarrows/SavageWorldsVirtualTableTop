import Ember from 'ember';

export default Ember.Controller.extend({

	needs: "chapter",
	chapter: Ember.computed.alias("controllers.chapter"),
	actions: {
		save:function() {
			var controller = this;
			this.model.save().then(function( newScene) {
				Ember.get(controller, 'flashes').success('Success!', 2000);
				var chapter = controller.get('chapter');
				chapter.model.get('scenes').addObject( newScene);
				chapter.model.save().then( function( savedChapter){
					controller.transitionToRoute('scene.edit', newScene);
				});
			});
		},
		cancel: function() {
			this.model.destroyRecord();
			this.transitionToRoute('chapter.edit', this.get('chapter'));
		}
	}
});
