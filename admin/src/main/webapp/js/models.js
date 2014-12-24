/**
 * 
 */

App.SkillDescription = DS.Model.extend({
	version:DS.attr('number'),
	name:DS.attr('string'),
	attribute:DS.attr('string'),
	description:DS.attr('string')
});