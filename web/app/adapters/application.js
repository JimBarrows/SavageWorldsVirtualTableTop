export default DS.RESTAdapter.extend({
    namespace: 'api',
//    host: 'http://localhost:8080',
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
