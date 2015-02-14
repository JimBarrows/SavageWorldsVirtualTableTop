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
			this.transitionToRoute('index');
		}
	}
});
