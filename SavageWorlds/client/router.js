Router.configure({
	layoutTemplate: 'SavageWorlds'
});

Router.route('/stories', {
	name: 'story.list',
	path: '/'
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