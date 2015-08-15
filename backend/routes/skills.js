module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var Skill = db.define('Skill', {
		rating: {
			type: sequelize.INTEGER,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		bonus: {
			type: sequelize.INTEGER,
			allowNull: false,
			validate: {
				notEmpty: true
			}
		}
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	var SkillDescription = db.models.SkillDescription;
	Skill.belongsTo(SkillDescription);
	
	router.get('/', function(req, res) {
		Skill.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'skill': data
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.skill;
		Skill.create(newRec)
		.then( function(data) {
			res.status(201).send({ skill: data}).end();	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.get('/:id', function(req, res) {
		Skill.findById( req.params.id).then(function(data){
			res.send({
				'skill':data
			});	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {error: error}).end();
		});
	});

	router.put('/:id', function(req, res) {
		Skill.findById( req.params.id)
			.then(function(data){
				data.updateAttributes(req.body.skill)
					.then(function(data) {
						res.send({
							'skill': data
						});
					});
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.delete('/:id', function(req, res) {
		Skill.findById( req.params.id).then(function(data) {
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
