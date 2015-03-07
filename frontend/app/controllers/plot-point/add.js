import Ember from 'ember';

export default Ember.Controller.extend({
	actions: {
		save:function() {
			var controller = this;
			this.model.save().then(function() {
				Ember.get(controller, 'flashes').success('Success!', 2000);
				controller.transitionToRoute('plot-point.edit', controller.model);
			});
		},
		cancel: function() {
			this.model.destroyRecord();
			this.transitionToRoute('index');
		}
	}
});
