module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var standardSkill = db.define('StandardSkill', {
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
		standardSkill.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'standardSkillDescription': data
			});
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.standardSkillDescription;
		standardSkill.create(newRec)
			.then( function(data) {
				res.status(201).send({ standardSkillDescription: data}).end();	
			})
			.catch( function(error){
				res.status(400, error);
			});
	});

	router.get('/:id', function(req, res) {
		standardSkill.findById( req.params.id).then(function(data){
			res.send({
				'standardSkillDescription':data
			});	
		})
		
	});

	router.put('/:id', function(req, res) {
		standardSkill.findById( req.params.id).then(function(data){
			data.updateAttributes(req.body.standardSkillDescription).then(function(data) {
				res.send({
					'standardSkillDescription': data
				});
			});
		});
	});

	router.delete('/:id', function(req, res) {
		standardSkill.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		});
	});

	return router;
}
