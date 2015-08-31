var express = require('express');
var router = express.Router();
var sequelize = require('sequelize');

var Edge = require("../models").Edge;

console.log("Building edges routes.");

router.get('/', function(req, res) {
	Edge.findAll({order: 'name ASC'})
		.then(function(data) {
			res.send({
				'edge': data
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
});

router.post('/', function(req, res) {
	var newRec = req.body.edge;
	Edge.create(newRec)
		.then( function(data) {
			res.status(201).send({ edge: data}).end();	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
});

router.get('/:id', function(req, res) {
	Edge.findById( req.params.id)
		.then(function(data){
			res.send({
				'edge':data
			});	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {error: error}).end();
		});
});

router.put('/:id', function(req, res) {
	Edge.findById( req.params.id)
		.then(function(data){
			data.updateAttributes(req.body.edge)
				.then(function(data) {
					res.send({
						'edge': data
					});
				});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
});

router.delete('/:id', function(req, res) {
	Edge.findById( req.params.id)
		.then(function(data) {
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
