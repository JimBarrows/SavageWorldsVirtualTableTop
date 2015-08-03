module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var RacialAbility = db.define('RacialAbility',{
		description: {
			type: sequelize.STRING,
			allowNull: false,
			validate: {
				notEmpty: true
			}
		},
		cost: {
			type: sequelize.INTEGER,
			defaultValue: 1,
			validate: {
				min: -3,
				max: 3
			}
		}
	});

	router.get('/', function(req, res) {
		console.log("Getting racial ability list");
		RacialAbility.findAll({order: 'description ASC'}).then(function(data) {
			res.send({
				'racialAbility': data
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.racialAbility;
		RacialAbility.create(newRec)
		.then( function(data) {
			res.status(201).send({ racialAbility: data}).end();	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.get('/:id', function(req, res) {
		RacialAbility.findById( req.params.id).then(function(data){
			res.send({
				'racialAbility':data
			});	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {error: error}).end();
		});
	});

	router.put('/:id', function(req, res) {
		RacialAbility.findById( req.params.id)
			.then(function(data){
				data.updateAttributes(req.body.racialAbility)
					.then(function(data) {
						res.send({
							'racialAbility': data
						});
					});
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.delete('/:id', function(req, res) {
		RacialAbility.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	return router;
}
