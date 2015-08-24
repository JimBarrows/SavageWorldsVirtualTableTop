import DS from 'ember-data';

export default DS.Model.extend({
	name: DS.attr('string'),
 	description: DS.attr('string'),
 //	story: DS.belongsTo('Story'),
 	scenes: DS.hasMany('Scene')
});
