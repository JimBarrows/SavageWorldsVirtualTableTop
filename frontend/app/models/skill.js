import DS from 'ember-data';

export default DS.Model.extend({
	description: DS.belongsTo('skill-description'),
	rating: DS.attr('number',{defaultValue:0}),
	bonus: DS.attr('number',{defaultValue:0}),
	increment: function( ) {
		var changeMade = false;
		if( this.get('rating') === 0) {
			this.set('rating', 4);
			changeMade = true;
		} else if( this.get( 'rating') === 4) {
			this.set( 'rating', 6);
			changeMade = true;
		} else if( this.get( 'rating') === 6) {
			this.set( 'rating', 8);
			changeMade = true;
		} else if( this.get( 'rating') === 8) {
			this.set( 'rating', 10);
			changeMade = true;
		} else if( this.get( 'rating') === 10) {
			this.set( 'rating', 12);
			changeMade = true;
		}
		return changeMade;
	},
	decrement: function() {
		var changeMade = false;
		if( this.get('rating') === 4) {
			this.set('rating', 0);
			changeMade = true;
		} else if( this.get( 'rating') === 6) {
			this.set( 'rating', 4);
			changeMade = true;
		} else if( this.get( 'rating') ===8) {
			this.set( 'rating', 6);
			changeMade = true;
		} else if( this.get( 'rating') ===10) {
			this.set( 'rating', 8);
			changeMade = true;
		} else if( this.get( 'rating') ===12) {
			this.set( 'rating', 10);
			changeMade = true;
		}
		return changeMade;
	}
});
