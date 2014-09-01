import Ember from 'ember';

export default Ember.Component.extend({
    actions: {

	add: function( rule){
	    this.get('selected').pushObject( rule);
	},
	remove: function( rule){
	    this.get('selected').removeObject( rule);
	}
    }
});
