import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr('string'),
    description: DS.attr('string'),
    era: DS.attr('string'),
    weight: DS.attr('number'),
    cost: DS.attr('number'),
    subType: DS.attr('string'),
    notes: DS.attr('string')
});
