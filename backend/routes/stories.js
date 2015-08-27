module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');
	var _ = require('underscore');
	var Promise = require("bluebird");


	var Story = db.define('Story', {
		name: {
			type: sequelize.STRING,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		description: {
			type: sequelize.STRING,
			allowNull: true
		}
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	var PlotPoint = db.models.PlotPoint;
	var Chapter = db.models.Chapter;
	var Scene = db.models.Scene;

	Story.hasMany(Chapter);
	Story.belongsTo(PlotPoint);

	router.get('/', function(req, res) {
		Promise.props({
			stories: Story.findAll({
					order: 'Story.name ASC',
					include:[{
						model: Chapter
					}]
				}),
			chapters: Chapter.findAll({
					order: 'Chapter.name ASC',
					include: [{
						model: Scene
					}]
				}),
			scenes: Scene.findAll()
		}).then(function( props){
			res.send({
				'Story': _.map( props.stories, function(story){
							return {
								id: story.id,
								name: story.name,
								description: story.description,
								chapters: _.pluck(story.Chapters,'id')
							};
						}),
				'Chapter': _.map( props.chapters, function( chapter) {
					return {
						id: chapter.id,
						name: chapter.name,
						description: chapter.description,
						scenes: _.pluck( chapter.Scenes, 'id')
					}
				}),
				'Scene': _.map( props.scenes, function( scene) {
					return {
						id: scene.id,
						name: scene.name,
						description: scene.description
					}
				})
			})
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.story;
		Story.create(newRec)
			.then( function(data) {
				res.status(201).send({ story: data}).end();	
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.get('/:id', function(req, res) {
		Story.findById( req.params.id).then(function(data){
			res.send({
				'story':data
			});	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {error: error}).end();
		});
	});

	router.put('/:id', function(req, res) {
		Story.findById( req.params.id)
			.then(function(story){
				console.log("found sotry" + story.id);
				addStoryIdToRecord( Chapter, req.body.story.chapters, story);
				story.updateAttributes(req.body.story)
					.then(function(data) {
						res.send({
							'story': data
						});
					});
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.delete('/:id', function(req, res) {
		Story.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	var addStoryIdToRecord = function( dbRecord, ids, plotPoint) {
		for( i=0; i< ids.length; i++) {
			console.log("Working id " + ids[i]);
			dbRecord.findById( ids[i])
				.then( function( foundRecord){
					foundRecord.updateAttributes({
						StoryId: plotPoint.id
					})
				})
				.catch( function(error) {
					console.log("Error adding plot point id to record."+ error);
				});
		}
	};
	return router;
}
