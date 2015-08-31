var express = require('express');
var router = express.Router();

var standardGear = require("../models").StandardGear;

console.log("Building standard gear routes.");

router.get('/', function(req, res) {
	standardGear.findAll({order: 'name ASC'}).then(function(data) {
		res.send({
			'standardGear': data
		});
	}).catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.standardGear;
	standardGear.create(newRec)
	.then( function(data) {
		res.status(201).send({ standardGear: data}).end();	
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.get('/:id', function(req, res) {
	standardGear.findById( req.params.id).then(function(data){
		res.send({
			'standardGear':data
		})
		.catch( function(error){
			res.status(400).send( {"errors": error}).end();
		});
	})
	
});

router.put('/:id', function(req, res) {
	standardGear.findById( req.params.id).then(function(data){
		data.updateAttributes(req.body.standardGear).then(function(data) {
			res.send({
				'standardGear': data
			});
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

router.delete('/:id', function(req, res) {
	standardGear.findById( req.params.id).then(function(data) {
		data.destroy().then(function(){
			res.status(204).end();	
		});
	})
	.catch( function(error){
		res.status(400).send( {"errors": error}).end();
	});
});

module.exports = router;
