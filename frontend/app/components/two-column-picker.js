import Ember from 'ember';

export default Ember.Component.extend({
    actions:{
    	selected: function( selectedItem, ops) {
    		this.sendAction('action', selectedItem, ops);
    	}
    }
});
