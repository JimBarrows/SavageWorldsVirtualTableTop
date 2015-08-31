var express = require('express');
var router = express.Router();

var standardEdge = require("../models").StandardEdge;

console.log("Building standard edges routes.");

router.get('/', function(req, res) {
	standardEdge.findAll({order: 'name ASC'}).then(function(data) {
		res.send({
			'standardEdge': data
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.standardEdge;
	standardEdge.create(newRec).then( function(data) {
		res.status(201).send({ standardEdge: data}).end();	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.get('/:id', function(req, res) {
	standardEdge.findById( req.params.id).then(function(data){
		res.send({
			'standardEdge':data
		});	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.put('/:id', function(req, res) {
	standardEdge.findById( req.params.id).then(function(data){
		data.updateAttributes(req.body.standardEdge).then(function(data) {
			res.send({
				'standardEdge': data
			});
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.delete('/:id', function(req, res) {
	standardEdge.findById( req.params.id).then(function(data) {
		data.destroy().then(function(){
			res.status(204).end();	
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

module.exports = router;
