Router.configure({
	layoutTemplate: 'SavageWorlds'
});

Router.onBeforeAction(function() {
	if (!Meteor.userId()) {
    	this.render('Marketing');
  	} else {
    	this.next();
	}
}, {
	except: ['home']
});

Router.route("/", {
	name: 'home',
	template: "Marketing"
});

Router.route('/stories', {
	name: 'story.list'
});

Router.route('/stories/add',{
	name: 'story.add',
	template: 'StoryForm'
});

Router.route('/stories/:_id/edit', {
	name: 'story.edit',
	template: 'StoryForm',
	data: function () {
    	return Stories.findOne({_id: this.params._id});
  	}
  });

Router.route('/stories/:_id', {
	name: 'story.view',
	template: 'Story',
	data: function () {
    	return Stories.findOne({_id: this.params._id});
  	}
  });

Router.route('/stories/scenes/:_id/edit', {
	name: 'scene.edit',
	template: 'SceneForm',
	data: function () {
    	return Scenes.findOne({_id: this.params._id});
  	}
  });

Router.route('/plotPoints',{
	name: 'plotPoint.list'
});

Router.route('/plotPoints/add',{
	name: 'plotPoint.add',
	template: 'PlotPointForm'
});

Router.route('/plotPoints/:_id/edit',{
	name: 'plotPoint.edit',
	template: 'PlotPointForm',
	data: function () {
    	return PlotPoints.findOne({_id: this.params._id});
  	}
})