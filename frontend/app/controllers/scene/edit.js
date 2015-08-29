import Ember from 'ember';

export default Ember.Controller.extend({

	actions: {
		save:function() {			
			var controller = this;
			this.model.save().then(function() {
				Ember.get(controller, 'flashes').success('Success!', 2000);				
			});
		},
		cancel: function() {
			this.model.rollback();
			this.transitionToRoute('chapter.edit', this.model.get('chapter').get('story'), this.model.get('chapter'));
		},
		addCharacter: function() {
			var newCharacter = this.store.createRecord('character');
			var chapter = this.model.get('chapter');
			var story = chapter.get('story');
			var plotPoint = story._relationships.plotPoint.record;//get('plotPoint');
			newCharacter.addPlotPoint( plotPoint);
		}
	}
});
