import Ember from 'ember';

var Router = Ember.Router.extend({
  location: SavageWorldsENV.locationType
});

Router.map(function() {
    this.route('signup');
    this.route('member');
    this.route('login');
    this.resource('plot-points', function(){
	this.route('create', {path:"/new"});
	this.route('read', {path:"/:plot-points_id"});
	this.route('update', {path:"/update/:plot-points_id"});
    });
});

export default Router;
