import Ember from 'ember';
import config from './config/environment';

var Router = Ember.Router.extend({
  location: config.locationType
});

Router.map(function() {
  this.resource("plot-point", function(){
  	this.route("add", {path:"/add"});
  	this.route("edit", {path:"/edit/:plot_point_id"});
  });
});

export default Router;
