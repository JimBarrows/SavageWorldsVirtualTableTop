module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');
	var _ = require('underscore');

	var Race = db.define('Race', {
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
		}
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	var RacialAbility = db.models.RacialAbility;

	Race.hasMany(RacialAbility);

	router.get('/', function(req, res) {
		Race.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'race': data
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.race;
		Race.create(newRec)
		.then( function(data) {
			addAbilityToRace( newRec.racialAbilities, data);
			res.status(201).send({ race: data}).end();	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.get('/:id', function(req, res) {
		Race.findById( req.params.id).then(function(data){
			res.send({
				'race':data
			});	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {error: error}).end();
		});
	});

	router.put('/:id', function(req, res) {
		var jsonRace = req.body.race;
		var raceId = req.params.id;
		Race.findById( raceId)
			.then(function(race){
				addAbilityToRace( jsonRace.racialAbilities, race);
				race.updateAttributes(jsonRace)
					.then(function(updatedRace) {
						res.send({
							'race': updatedRace
						});
					});
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.delete('/:id', function(req, res) {
		Race.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	var addAbilityToRace = function(  racialAbilityIds, race) {
		_.each(racialAbilityIds, function( racialAbilityId){
			RacialAbility.findById( racialAbilityId)
				.then( function( racialAbility){
					racialAbility.updateAttributes({
						RaceId: race.id
					})
				})
				.catch( function(error) {
					console.log("Error adding racialAbility id to race."+ error);
				});
		});
	};

	return router;
}
