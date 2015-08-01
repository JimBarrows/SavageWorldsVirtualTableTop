module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');
	var _ = require('underscore');

	var PlotPoint = db.define('PlotPoint', {
		name: {
			type: sequelize.STRING,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		description: {
			type: sequelize.STRING,
			allowNull: false,
			validate: {
				notEmpty: true
			}
		},
		bloodAndGuts: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		bornAHero: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		criticalFailures: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		fantatics: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		grittyDamage: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		heroesNeverDie: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		highAdventure: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		jokersWild: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		multipleLanguages: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		noPowerPoints: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		skillSpecialization: {
			type: sequelize.BOOLEAN,
			defaultValue: false
		},
		startingAttributePoints: {
			type: sequelize.INTEGER,
			defaultValue: 5
		},
		startingSkillPoints: {
			type: sequelize.INTEGER,
			defaultValue: 15
		},
		startingMajorHindrances: {
			type: sequelize.INTEGER,
			defaultValue: 1
		},
		startingMinorHindrances: {
			type: sequelize.INTEGER,
			defaultValue: 2
		},
		startingCash: {
			type: sequelize.INTEGER,
			defaultValue: 500
		}
	}, {
		freezeTableName: true
	});

var SkillDescription = db.models.SkillDescription;
var Hindrance = db.models.Hindrance;
var Edge = db.models.Edge;

PlotPoint.hasMany(SkillDescription);
PlotPoint.hasMany(Hindrance);
PlotPoint.hasMany(Edge);

router.get('/', function(req, res) {
	PlotPoint.findAll({
		include: [{
			model: SkillDescription
		}, {
			model: Hindrance
		},{
			model: Edge
		}]
	})
	.then(function(plotPointList) {
		var plotPoints =[];
		var skillDescriptions=[];
		var hindrances=[];
		var edges=[];
		_.each(plotPointList, function(plotPoint) {
			var jsonPlotPoint = {
				"id": plotPoint.id,
				"name": plotPoint.name,
				"description": plotPoint.description,
				"bloodAndGuts": plotPoint.bloodAndGuts,
				"bornAHero": plotPoint.bornAHero,
				"criticalFailures": plotPoint.criticalFailures,
				"fanatics": plotPoint.fanatics,
				"grittyDamage": plotPoint.grittyDamage,
				"heroesNeverDie": plotPoint.heroesNeverDie,
				"highAdventure": plotPoint.highAdventure,
				"jokersWild": plotPoint.multipleLanguages,
				"multipleLanguages": plotPoint.multipleLanguages,
				"noPowerPoints": plotPoint.noPowerPoints,
				"skillSpecialization": plotPoint.skillSpecialization,
				"startingAttributePoints": plotPoint.startingAttributePoints,
				"startingSkillPoints": plotPoint.startingSkillPoints,
				"startingMajorHindrances": plotPoint.startingMajorHindrances,
				"startingMinorHindrances": plotPoint.startingMinorHindrances,
				"startingCash": plotPoint.startingCash,
				"skillDescriptions": [],
				"hindrances": [],
				"edges": [],
			};
			_.each(plotPoint.SkillDescriptions, function(skill){
				jsonPlotPoint.skillDescriptions.push(skill.id);
				skillDescriptions.push( skill);
			});
			_.each(plotPoint.Hindrances, function(hindrance){
				jsonPlotPoint.hindrances.push(hindrance.id);
				hindrances.push(hindrance);
			});
			_.each(plotPoint.Edges, function(edge){
				jsonPlotPoint.edges.push(edge.id);
				edges.push(edge);
			});
			plotPoints.push(jsonPlotPoint);
		});
		res.send({
			'PlotPoint': plotPoints,
			'SkillDescriptions': skillDescriptions,
			'Hindrances' : hindrances,
			'Edges' : edges
		});
	})
	.catch( function(error){
		console.log("Error getting plot point. " + error)
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.plotPoint;
	PlotPoint.create(newRec)
			.then( function(data) {
				updateSkillDesciription( newRec.skillDescriptions, data);
				updateHindrances( newRec.hindrances, data);
				addPlotPointIdToRecord( Edge, newRec.edges, data);
				res.status(201).send({ PlotPoint: data}).end();	
			})
			.catch( function(error){
				console.log("Error creating new plot point: " + error);
				res.status(400).send( {"errors": error}).end();
			});
});

var updateSkillDesciription = function( ids, plotPoint) {
	for( i=0; i< ids.length; i++) {
		SkillDescription.findById(ids[i])
			.then( function( record){
				record.updateAttributes({
					PlotPointId: plotPoint.id
				})
			})
			.catch( function(error) {
				console.log("Error updating skill description" + error);
			})
	}
};

var updateHindrances = function( ids, plotPoint) {
	for( i=0; i< ids.length; i++) {
		Hindrance.findById(ids[i])
			.then( function( record){
				record.updateAttributes({
					PlotPointId: plotPoint.id
				})
			})
			.catch( function(error) {
				console.log("Error updating hindrances."+ error);
			})
	}
};

var addPlotPointIdToRecord = function( dbRecord, ids, plotPoint) {
	for( i=0; i< ids.length; i++) {
		dbRecord.findById( ids[i])
			.then( function( foundRecord){
				foundRecord.updateAttributes({
					PlotPointId: plotPoint.id
				})
			})
			.catch( function(error) {
				console.log("Error adding plot point id to record."+ error);
			})
	}
};
router.get('/:id', function(req, res) {
	PlotPoint.findById( req.params.id)
	.then(function(data){
		res.send({
			'PlotPoint':data
		});	
	})
	.catch( function(error){
		console.log("Error getting plot point" + error);
		res.status(400).send( {"errors": error}).end();
	});
});

router.put('/:id', function(req, res) {
	var plotPointId = req.params.id;
	var modifiedPlotPoint = req.body.plotPoint;

	PlotPoint.findById( plotPointId)
		.then(function(originalPlotPoint){
			updateSkillDesciription( modifiedPlotPoint.skillDescriptions, originalPlotPoint);
			updateHindrances( modifiedPlotPoint.hindrances, originalPlotPoint);
			addPlotPointIdToRecord( Edge, modifiedPlotPoint.edges, originalPlotPoint);
			originalPlotPoint.updateAttributes(modifiedPlotPoint)
				.then(function(newPlotPoint) {
					res.send({
						'PlotPoint': newPlotPoint
					});
				})
				.catch( function(error){
					console.log("Error updating plot point.  " + error);
					res.status(400).send( {"errors": error}).end();
				});
		})
		.catch( function( error){
			console.log("Error finding plot point for updating.  " + error);
			res.status(400).send( {'errors':error}).end();
		});
});

router.delete('/:id', function(req, res) {
	PlotPoint.findById( req.params.id)
		.then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		})
		.catch( function(error){
			console.log("Error deleting plot point" + error);
			res.status(400).send( {"errors": error}).end();
		});
});

return router;
}
