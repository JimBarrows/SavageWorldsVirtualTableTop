import Ember from 'ember';

export function hindranceAvailable(input) {
  return true;
}

export default Ember.Handlebars.makeBoundHelper(hindranceAvailable);
