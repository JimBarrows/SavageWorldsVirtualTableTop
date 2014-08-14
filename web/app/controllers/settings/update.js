import Ember from 'ember';

export default Ember.Controller.extend({
    settingRules: [],
    actions:{
	save:function(){
	    var setting = this.get('model');
	    setting.set('userId', this.get('session.userId'));
	    var self = this;
	    setting.save().then(
		function( post){
		    self.transitionToRoute("/settings", post);
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
