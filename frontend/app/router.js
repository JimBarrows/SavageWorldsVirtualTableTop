import Ember from 'ember';
import config from './config/environment';

var Router = Ember.Router.extend({
  location: config.locationType
});

Router.map(function() {
	this.resource("plot-points", function(){
		this.route("add");
		this.resource('plot-point', { path: '/:id' }, function() {
			this.route("edit");
		});
    });
	this.resource("standard-races", function() {
		this.route("add");
		this.resource('standard-race', { path: '/:id'}, function(){
			this.route('edit');
		});
	});
});

export default Router;
