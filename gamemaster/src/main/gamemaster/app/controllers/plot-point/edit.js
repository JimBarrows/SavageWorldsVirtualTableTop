import Ember from 'ember';

export default Ember.Controller.extend({
	actions: {
		save:function() {
			this.model.save();
		},
		cancel: function() {
			this.model.rollback();
			this.transitionToRoute('index');
		}
	}
});
