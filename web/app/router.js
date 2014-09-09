import Ember from 'ember';

var Router = Ember.Router.extend({
  location: SavageWorldsENV.locationType
});

Router.map(function() {
    this.route('signup');
    this.route('member');
    this.route('login');
    this.resource('settings', function(){
	this.route('create', {path:"/new"});
	this.route('read', {path:"/:settings_id"});
	this.route('update', {path:"/update/:settings_id"});
    });

});

export default Router;
