import Ember from 'ember';

var Router = Ember.Router.extend({
  location: SavageWorldsENV.locationType
});

Router.map(function() {
    this.route('signup');
    this.route('member');
});

export default Router;
