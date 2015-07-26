module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var plotPoint = db.define('PlotPoint', {
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
		freezeTableName: true // Model tableName will be the same as the model name
	});

router.get('/', function(req, res) {
	plotPoint.findAll({order: 'name ASC'})
	.then(function(data) {
		res.send({
			'plotPoint': data
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.plotPoint;
	plotPoint.create(newRec)
	.then( function(data) {
		res.status(201).send({ plotPoint: data}).end();	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.get('/:id', function(req, res) {
	plotPoint.findById( req.params.id).then(function(data){
		res.send({
			'plotPoint':data
		});	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
	
});

router.put('/:id', function(req, res) {
	plotPoint.findById( req.params.id).then(function(data){
		data.updateAttributes(req.body.plotPoint).then(function(data) {
			res.send({
				'plotPoint': data
			});
		})
		.catch( function(error){
			res.status(400).send( {"errors": error}).end();
		});
	});
});

router.delete('/:id', function(req, res) {
	plotPoint.findById( req.params.id).then(function(data) {
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
