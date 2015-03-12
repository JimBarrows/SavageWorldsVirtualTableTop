import Ember from 'ember';

export default Ember.Component.extend({
	actions: {
		increment: function( model, attribute) {
			if( model.get( attribute) === 4) {
				model.set( attribute, 6);
				this.sendAction("onIncrease");
			} else if( model.get( attribute) ===6) {
				model.set( attribute, 8);
				this.sendAction("onIncrease");
			} else if( model.get( attribute) ===8) {
				model.set( attribute, 10);
				this.sendAction("onIncrease");
			} else if( model.get( attribute) ===10) {
				model.set( attribute, 12);
				this.sendAction("onIncrease");
			}
		},
		decrement: function( model, attribute) {
			if( model.get( attribute) === 6) {
				model.set( attribute, 4);
				this.sendAction("onDecrease");
			} else if( model.get( attribute) ===8) {
				model.set( attribute, 6);
				this.sendAction("onDecrease");
			} else if( model.get( attribute) ===10) {
				model.set( attribute, 8);
				this.sendAction("onDecrease");
			} else if( model.get( attribute) ===12) {
				model.set( attribute, 10);
				this.sendAction("onDecrease");
			}
		}
	}
});
