import Ember from 'ember';

export default Ember.Route.extend({
	model: function() {
		var storyIndexController = this.controllerFor('stories.index');
		var selectedPlotPoint = storyIndexController.get("selectedPlotPoint");
		var newStory = this.store.createRecord('story');
		newStory.set('plotPoint', selectedPlotPoint);
		return newStory;
	}
});
