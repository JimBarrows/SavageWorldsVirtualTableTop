import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr('string'),
    description: DS.attr('string'),
    era: DS.attr('string'),
    weight: DS.attr('string'),
    cost: DS.attr('string'),
    subType: DS.attr('string'),
    notes: DS.attr('string')
});
