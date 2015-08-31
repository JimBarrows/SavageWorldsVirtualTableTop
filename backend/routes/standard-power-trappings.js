var express = require('express');
var router = express.Router();

var standardPowerTrapping = require("../models").StandardPowerTrapping;

console.log("Building standard power trappings routes.");

router.get('/', function(req, res) {
	standardPowerTrapping.findAll({order: 'name ASC'}).then(function(data) {
		res.send({
			'standardPowerTrapping': data
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.standardPowerTrapping;
	console.log("name: " + newRec.name);
	standardPowerTrapping.create(newRec)
	.then( function(data) {
		res.status(201).send({ standardPowerTrapping: data}).end();	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.get('/:id', function(req, res) {
	standardPowerTrapping.findById( req.params.id).then(function(data){
		res.send({
			'standardPowerTrapping':data
		});	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.put('/:id', function(req, res) {
	standardPowerTrapping.findById( req.params.id).then(function(data){
		data.updateAttributes(req.body.standardPowerTrapping).then(function(data) {
			res.send({
				'standardPowerTrapping': data
			});
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.delete('/:id', function(req, res) {
	standardPowerTrapping.findById( req.params.id).then(function(data) {
		data.destroy().then(function(){
			res.status(204).end();	
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

module.exports = router;
