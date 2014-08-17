import Ember from 'ember';
import AuthenticatedRouteMixin from 'simple-auth/mixins/authenticated-route-mixin';

export default Ember.Route.extend(AuthenticatedRouteMixin,{
    setupController: function(controller, model) {
	controller.set('model', model);
	controller.set('settingRules', this.store.find('setting-rule'));
	controller.set('skillDescriptions', this.store.find('skill-description'));
    }
});
