module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var SkillDescription = db.define('SkillDescription', {
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
		attribute: {
			type: sequelize.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			},
			values: ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor']
		}
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	router.get('/', function(req, res) {
		SkillDescription.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'skillDescription': data
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.skillDescription;
		console.log("newRec: " + newRec.name);
		SkillDescription.create(newRec)
		.then( function(data) {
			res.status(201).send({ skillDescription: data}).end();	
		})
		.catch( function(error){
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.get('/:id', function(req, res) {
		SkillDescription.findById( req.params.id).then(function(data){
			res.send({
				'skillDescription':data
			});	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {error: error}).end();
		});
	});

	router.put('/:id', function(req, res) {
		SkillDescription.findById( req.params.id)
			.then(function(data){
				data.updateAttributes(req.body.skillDescription)
					.then(function(data) {
						res.send({
							'skillDescription': data
						});
					});
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.delete('/:id', function(req, res) {
		SkillDescription.findById( req.params.id).then(function(data) {
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
