var express = require('express');
var router = express.Router();

var SkillDescription = require("../models").SkillDescription;

console.log("Building skill description routes.");

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

module.exports = router;
