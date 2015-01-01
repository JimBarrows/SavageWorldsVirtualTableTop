App = Ember.Application.create();

App.HtmlTextAreaComponent = Ember.TextArea.extend({
    didInsertElement: function() {
        this._super();
        var self = this;
        var elementId = self.get('elementId');

        var edit = CKEDITOR.replace( elementId, {
            extraPlugins : 'autogrow',
            autoGrow_maxHeight : 800,
            // Remove the Resize plugin as it does not make sense to use it in conjunction with the AutoGrow plugin.
            removePlugins : 'resize'
        });

        edit.on('blur', function(e) {
                if (e.editor.checkDirty()) {
                        self.set('value', edit.getData() );
                }
        });
    }
});

App.ApplicationAdapter = DS.RESTAdapter.extend({
	namespace : 'admin/api',
	ajaxError : function(jqXHR) {

		var error = this._super(jqXHR);

		if (jqXHR && jqXHR.status === 422) {
			var response = Ember.$.parseJSON(jqXHR.responseText), errors = {};

			if (response.errors !== undefined) {
				var jsonErrors = response.errors;

				Ember.EnumerableUtils.forEach(Ember.keys(jsonErrors), function(
						key) {

					errors[Ember.String.camelize(key)] = jsonErrors[key];
				});
			}
			return new DS.InvalidError(errors);
		} else {
			return error;
		}
	}
});

$.ajaxSetup({
	contentType : "application/json"
});

App.Router.map(function() {
	this.resource('skilldescriptions', function() {
		this.route('create', {
			path : "/new"
		});
		this.route('edit', {
			path : "/:skill_description_id"
		});
	});
	this.resource('armordescriptions', function() {
		this.route('create', {
			path : "/new"
		});
		this.route('edit', {
			path : "/:armor_description_id"
		});
	});
	this.resource('edgetypes', function() {
		this.route('create', {
			path : "/new"
		});
		this.route('edit', {
			path : "/:edge_type_id"
		});
	});
});
