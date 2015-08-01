module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var Power = db.define('Power', {
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
		powerPoints: {
			type: sequelize.INTEGER,
			allowNull: false,
			defaultValue: 1,
			validate: {
				min: 1
			}
		},
		rank:{
			type: sequelize.ENUM,
			values: ['Novice','Seasoned', 'Veteran', 'Heroic', 'Legendary'],
			allowNull: false,
			defaultValue: 'Novice'
		},
		duration:{
			type: sequelize.STRING,
			defaultValue: 'Instant'
		},
		maintenanceCost:{
			type: sequelize.STRING,
			allowNull: true
		}

	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	router.get('/', function(req, res) {
		Power.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'power': data
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.power;
		Power.create(newRec)
		.then( function(data) {
			res.status(201).send({ power: data}).end();	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.get('/:id', function(req, res) {
		Power.findById( req.params.id).then(function(data){
			res.send({
				'power':data
			});	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {error: error}).end();
		});
	});

	router.put('/:id', function(req, res) {
		Power.findById( req.params.id)
			.then(function(data){
				data.updateAttributes(req.body.power)
					.then(function(data) {
						res.send({
							'power': data
						});
					});
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.delete('/:id', function(req, res) {
		Power.findById( req.params.id).then(function(data) {
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
