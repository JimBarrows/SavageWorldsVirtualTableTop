import DS from "ember-data";
import Ember from 'ember';

export default DS.RESTAdapter.extend({
    namespace: 'api',
    ajaxError: function(jqXHR) {
        var error = this._super(jqXHR);
	
        if (jqXHR && jqXHR.status === 422) {
            var jsonErrors = Ember.$.parseJSON(jqXHR.responseText)["errors"];
	    
            return new DS.InvalidError(jsonErrors);
        } else {
            return error;
        }
    }
});