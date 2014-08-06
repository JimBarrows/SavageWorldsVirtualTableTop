import Ember from 'ember';
import AuthenticatedRouteMixin from 'simple-auth/mixins/authenticated-route-mixin';

export default Ember.Route.extend(AuthenticatedRouteMixin,{
   
    model: function() {
	return this.store.createRecord('plot-point');
    },

    setupController: function(controller, model) {
	this.controllerFor('plot-points.create').setProperties({isNew: false,model:model});
    },

    renderTemplate: function() {
	this.render('plot-points.create',{into:'application'});
    }
});
