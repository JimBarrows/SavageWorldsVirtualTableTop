import Ember from 'ember';

export default Ember.Route.extend({	
	model: function(params) {
	 
	 	return this.store.find('standard-power', params.id);	 
	 }
});