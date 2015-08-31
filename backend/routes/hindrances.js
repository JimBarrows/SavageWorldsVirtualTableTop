var express = require('express');
var router = express.Router();
var sequelize = require('sequelize');

var Hindrance = require("../models").Hindrance;

console.log("Building hindrances routes.");

router.get('/', function(req, res) {
	Hindrance.findAll({order: 'name ASC'}).then(function(data) {
		res.send({
			'hindrance': data
		});
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.hindrance;
	Hindrance.create(newRec)
	.then( function(data) {
		res.status(201).send({ hindrance: data}).end();	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.get('/:id', function(req, res) {
	Hindrance.findById( req.params.id).then(function(data){
		res.send({
			'hindrance':data
		});	
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {error: error}).end();
	});
});

router.put('/:id', function(req, res) {
	Hindrance.findById( req.params.id)
		.then(function(data){
			data.updateAttributes(req.body.hindrance)
				.then(function(data) {
					res.send({
						'hindrance': data
					});
				});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
});

router.delete('/:id', function(req, res) {
	Hindrance.findById( req.params.id).then(function(data) {
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
