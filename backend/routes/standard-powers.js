var express = require('express');
var router = express.Router();

var standardPower = require("../models").standardPower;

console.log("Building standard power routes.");

router.get('/', function(req, res) {
	standardPower.findAll({order: 'name ASC'}).then(function(data) {
		res.send({
			'standardPower': data
		});
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.standardPower;
	standardPower.create(newRec)
	.then( function(data) {
		res.status(201).send({ standardPower: data}).end();	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.get('/:id', function(req, res) {
	standardPower.findById( req.params.id).then(function(data){
		res.send({
			'standardPower':data
		});	
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {"errors": error}).end();
	});
});

router.put('/:id', function(req, res) {
	standardPower.findById( req.params.id).then(function(data){
		data.updateAttributes(req.body.standardPower).then(function(data) {
			res.send({
				'standardPower': data
			});
		});
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {"errors": error}).end();
	});
});

router.delete('/:id', function(req, res) {
	standardPower.findById( req.params.id).then(function(data) {
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
