import DS from 'ember-data';

export default DS.Model.extend({
	description: DS.belongsTo('skill-description'),
	rating: DS.attr('number'),
	bonus: DS.attr('number')  
});
