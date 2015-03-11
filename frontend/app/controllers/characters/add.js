import Ember from 'ember';

export default Ember.Controller.extend({
	actions: {
		save:function() {
			var controller = this;
			this.model.save().then(function( newCharacter) {
				Ember.get(controller, 'flashes').success('Success!', 2000);
				controller.transitionToRoute('character.edit', newCharacter);
			});
		},
		cancel: function() {
			this.model.destroyRecord();
			this.transitionToRoute('characters');
		}
	}
});
