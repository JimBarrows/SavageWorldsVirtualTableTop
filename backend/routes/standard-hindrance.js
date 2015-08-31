var express = require('express');
var router = express.Router();

var standardHindrance = require("../models").StandardHindrance;

console.log("Building stnadard hindrance routes.");

router.get('/', function(req, res) {
	standardHindrance.findAll({order: 'name ASC'}).then(function(data) {
		res.send({
			'standardHindrance': data
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.standardHindrance;
	standardHindrance.create(newRec).then( function(data) {
		res.status(201).send({ standardHindrance: data}).end();	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.get('/:id', function(req, res) {
	standardHindrance.findById( req.params.id).then(function(data){
		res.send({
			'standardHindrance':data
		});	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
	
});

router.put('/:id', function(req, res) {
	standardHindrance.findById( req.params.id).then(function(data){
		data.updateAttributes(req.body.standardHindrance).then(function(data) {
			res.send({
				'standardHindrance': data
			});
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.delete('/:id', function(req, res) {
	standardHindrance.findById( req.params.id).then(function(data) {
		data.destroy().then(function(){
			res.status(204).end();	
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

module.exports = router;
