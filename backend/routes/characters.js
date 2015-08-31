var express = require('express');
var router = express.Router();
var sequelize = require('sequelize');

var Character = require("../models").Character;

console.log("Building characters routes.");

router.get('/', function(req, res) {
	Character.findAll({order: 'name ASC'}).then(function(data) {
		res.send({
			'character': data
		});
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.character;
	Character.create(newRec)
		.then( function(data) {
			res.status(201).send({ character: data}).end();	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
});

router.get('/:id', function(req, res) {
	Character.findById( req.params.id).then(function(data){
		res.send({
			'character':data
		});	
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {error: error}).end();
	});
});

router.put('/:id', function(req, res) {
	Character.findById( req.params.id)
		.then(function(data){
			data.updateAttributes(req.body.character)
				.then(function(data) {
					res.send({
						'character': data
					});
				});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
});

router.delete('/:id', function(req, res) {
	Character.findById( req.params.id).then(function(data) {
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
