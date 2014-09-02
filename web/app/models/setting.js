import DS from 'ember-data';

export default DS.Model.extend({
    userId: DS.attr('string'),
    name: DS.attr('string'),
    settingRules: DS.hasMany('setting-rule',{async:true}),
    skillDescriptions: DS.hasMany('skill-description',{async:true}),
    hindrances: DS.hasMany('hindrance',{async:true})
});
