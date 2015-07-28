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

PlotPoint.hasMany(SkillDescription);

router.get('/', function(req, res) {
	PlotPoint.findAll({
		include: [{
			model: SkillDescription
		}]
	})
	.then(function(plotPointList) {
		var plotPoints =[];
		var skillDescriptions=[];
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
				"skillDescriptions": []
			};
			_.each(plotPoint.SkillDescriptions, function(skill){
				jsonPlotPoint.skillDescriptions.push(skill.id);
				skillDescriptions.push( skill);
			});
			plotPoints.push(jsonPlotPoint);
		});
		res.send({
			'PlotPoint': plotPoints,
			'SkillDescriptions': skillDescriptions
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.plotPoint;
	var skillIds = newRec.skillDescriptions;

	PlotPoint.create(newRec)
	.then( function(data) {
		updateSkillDesciription( skillIds, data);
		res.status(201).send({ PlotPoint: data}).end();	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

var updateSkillDesciription = function( skillIds, plotPoint) {
	debugger;
	for( i=0; i< skillIds.length; i++) {
		SkillDescription.findById(skillIds[i])
		.then( function( skill){
			skill.updateAttributes({
				PlotPointId: plotPoint.id
			})
		})
		.catch( function(error) {
			console.log(error);
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
		res.status(400).send( {"errors": error}).end();
	});
});

router.put('/:id', function(req, res) {
	var plotPointId = req.params.id;
	var updatePlotPoint = req.body.plotPoint;
	var skillIds = updatePlotPoint.skillDescriptions;
	debugger;
	PlotPoint.findById( plotPointId)
	.then(function(data){
		updateSkillDesciription(skillIds, data);
		data.updateAttributes(updatePlotPoint).then(function(data) {
			res.send({
				'PlotPoint': data
			});
		})
		.catch( function(error){
			res.status(400).send( {"errors": error}).end();
		});
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
		res.status(400).send( {"errors": error}).end();
	});
});

return router;
}
