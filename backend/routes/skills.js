var express = require('express');
var router = express.Router();

var models = require("../models");
var Skill = models.Skill;
var SkillDescription = models.SkillDescription;

console.log("Building skills routes.");

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

module.exports = router;
