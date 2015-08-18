import DS from 'ember-data';

export default DS.Model.extend({
	name: DS.attr('string'),
 	description: DS.attr('string'),
 	chapters: DS.hasMany('Chapter'),
 	plotPoint: DS.belongsTo('plot-point')
});
