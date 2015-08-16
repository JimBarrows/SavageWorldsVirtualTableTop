import Ember from 'ember';

export default Ember.Route.extend({	
	setupController: function(controller,model){
		this._super(controller, model);
		model.get('racialAbilities').forEach(function (racialAbility) {
				controller.incrementProperty( "totalCost", racialAbility.get('cost'))
			});	
	}
});
