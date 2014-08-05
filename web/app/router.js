import Ember from 'ember';

var Router = Ember.Router.extend({
  location: SavageWorldsENV.locationType
});

Router.map(function() {
    this.route('signup');
    this.route('member');
    this.route('login');
    this.route('plot-points');
  this.route('plot-point');
});

export default Router;
