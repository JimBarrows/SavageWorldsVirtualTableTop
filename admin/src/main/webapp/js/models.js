/**
 * 
 */

App.SkillDescription = DS.Model.extend({
	version:DS.attr('number', {defaultValue: 0}),
	name:DS.attr('string'),
	attribute:DS.attr('string'),
	description:DS.attr('string')
});

App.ArmorDescription = DS.Model.extend({
	version   : DS.attr('number', {defaultValue: 0}),
	name      : DS.attr('string'),
	armor     : DS.attr('number'),
	vsBullets : DS.attr('number'),
	weight    : DS.attr('number'),
	cost      : DS.attr('number'),
	notes     : DS.attr('string'),
	era		  : DS.attr('string')
});