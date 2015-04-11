import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr('string'),
    severity: DS.attr('string'),
    description: DS.attr('string'),
    isMajor: function() {
    	return this.get('severity') === 'Major' ;
    }.property('severity'),
    isMinor: function() {
    	return this.get('severity') === 'Minor' ;
    }.property('severity'),
    isMajorOrMinor: function() {
    	return this.get('severity') === 'Major or Minor';
    }.property('severity')
});
