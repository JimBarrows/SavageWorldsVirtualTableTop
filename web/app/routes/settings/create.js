import Ember from 'ember';
import AuthenticatedRouteMixin from 'simple-auth/mixins/authenticated-route-mixin';

export default Ember.Route.extend(AuthenticatedRouteMixin,{
   
    model: function() {
	return this.store.createRecord('setting');
    },

    setupController: function(controller, model) {
	this.controllerFor('settings.create').setProperties({isNew: false,model:model});
    },

    renderTemplate: function() {
	this.render('settings.create',{into:'application'});
    }
});
