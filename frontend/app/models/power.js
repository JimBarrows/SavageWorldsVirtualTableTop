import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr('string'),
    description: DS.attr('string'),
    powerPoints: DS.attr('number',{defaultValue:1}),
    rank: DS.attr('string',{defaultValue:'Novice'}),
    range: DS.attr('string',{defaultValue:'Touch'}),
    duration: DS.attr('number',{defaultValue:0}),
    maintenanceCost: DS.attr('number',{defaultValue:0})
});
