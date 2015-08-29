import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr('string'),
    description: DS.attr('string'),
    type: DS.attr('string'), //Player, Wild Card, Extra
    rank: DS.attr('string', {defaultValue: 'Novice'}), //Novice,
    plotPoint: DS.belongsTo('plot-point'),
    agility: DS.attr('number', {defaultValue: 4}),
    smarts: DS.attr('number', {defaultValue: 4}),
    strength: DS.attr('number', {defaultValue: 4}),
    spirit: DS.attr('number', {defaultValue: 4}),
    vigor: DS.attr('number', {defaultValue: 4}),
    cash: DS.attr('number', {defaultValue: 500}),
    skills: DS.hasMany('skill'),
    hindrances: DS.hasMany('hindrance'),
    edges: DS.hasMany('edge'),
    powers: DS.hasMany('power'),
    gear: DS.hasMany('gear'),

    addPlotPoint: function(plotPoint) {
        var disCharacter = this;
        plotPoint.get('skillDescriptions').forEach(function( skillDescription){
            var newSkill = route.store.createRecord('skill',{
                description: skillDescription
            });
            newSkill.save().then(function(savedSkill){
                disCharacter.get('skills').addRecord(savedSkill);
            });
            
        });
        disCharacter.set('plotPoint', plotPoint);
    }
});
