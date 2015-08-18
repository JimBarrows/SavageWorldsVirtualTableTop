module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');
	var _ = require('underscore');

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

	// Story.belongsTo( PlotPoint);
	Story.hasMany(Chapter);

	router.get('/', function(req, res) {
		Story.findAll({
			order: 'Story.name ASC',
			include:[{
				model: Chapter
			}]
		}).then(function(storyList) {
			res.send(buildSideLoadedResponse(storyList));
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
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

	buildSideLoadedResponse = function( storyList) {
		var sideLoadedResponse ={
			'Story':[],
			'Chapters':[]
		};
		console.log("storyList; " + storyList);
		_.each(storyList, function(story){
			console.log("story: " + story.id);
			var storyJson = storyRecordToJson( story);	
			storyJson.chapters = extractIdList(story.Chapters);
			sideLoadedResponse.Story.push( storyJson);
			sideLoadedResponse.Chapters = sideLoadedResponse.Chapters.concat(story.Chapters);
		});
		return sideLoadedResponse;
	};

	storyRecordToJson = function(story) {
		return { 
			"id": story.id,
			"name": story.name,
			"description": story.description,
			"chapters": []
		}
	};
	var extractIdList = function( record) {
		var idList = []
		_.each(record, function( rec){
			idList.push(rec.id);
		});
		return idList;
	};

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
