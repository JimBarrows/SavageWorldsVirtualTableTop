Router.configure({
	layoutTemplate: 'SavageWorlds'
});

Router.route('/storyList', {
	path: '/'
});

Router.route('/storyAdd',{
	name: 'story.add',
	template: 'StoryForm'
});

Router.route('/storyEdit/:_id', {
	name: 'story.edit',
	template: 'StoryForm',
	data: function () {
    	return Stories.findOne({_id: this.params._id});
  	}
  });