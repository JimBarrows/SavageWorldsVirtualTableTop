import Ember from 'ember';

export default Ember.Controller.extend({
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
	}
    }
});
