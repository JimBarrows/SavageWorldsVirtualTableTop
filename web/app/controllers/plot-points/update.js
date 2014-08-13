import Ember from 'ember';

export default Ember.Controller.extend({
    settingRules: [],
    actions:{
	save:function(){
	    var plotPoint = this.get('model');
	    plotPoint.set('userId', this.get('session.userId'));
	    var self = this;
	    plotPoint.save().then(
		function( post){
		    self.transitionToRoute("/plot-points", post);
		},
		function( error) {
		});
	},
	addRule: function( rule){
	    this.get('model').get('settingRules').pushObject( rule);
	},
	removeRule: function( rule){
	    this.get('model').get('settingRules').removeObject( rule);
	}
    }
});
