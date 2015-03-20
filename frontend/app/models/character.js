import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr('string'),
    description: DS.attr('string'),
    plotPoint: DS.belongsTo('plot-point'),
    agility: DS.attr('number', {defaultValue: 4}),
    smarts: DS.attr('number', {defaultValue: 4}),
    strength: DS.attr('number', {defaultValue: 4}),
    spirit: DS.attr('number', {defaultValue: 4}),
    vigor: DS.attr('number', {defaultValue: 4}),
    cash: DS.attr('number', {defaultValue: 500})
});
