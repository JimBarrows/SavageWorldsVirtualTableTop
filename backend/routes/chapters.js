var express = require('express');
var router = express.Router();

var Chapter = require("../models").Chapter;

console.log("Building chapters routes.");

router.get('/', function(req, res) {
	Chapter.findAll({
		order: 'Chapter.name ASC',
		include: [{
			model: Scene
		}]
	}).then(function(data) {
		res.send({
			'chapter': data
		});
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {"errors": error}).end();
	});
});

router.post('/', function(req, res) {
	var newRec = req.body.chapter;
	Chapter.create(newRec)
		.then( function(data) {
			res.status(201).send({ chapter: data}).end();	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
});

router.get('/:id', function(req, res) {
	Chapter.findById( req.params.id).then(function(data){
		res.send({
			'chapter':data
		});	
	})
	.catch( function(error){
		console.log("error: " + error);
		res.status(400).send( {error: error}).end();
	});
});

router.put('/:id', function(req, res) {
	Chapter.findById( req.params.id)
		.then(function(data){
			data.updateAttributes(req.body.chapter)
				.then(function(data) {
					Scene.update({
						ChapterId: data.id
					}, {
						where: { id : req.body.chapter.scenes} 
					});
					res.send({
						'chapter': data
					});
				});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
});

router.delete('/:id', function(req, res) {
	Chapter.findById( req.params.id).then(function(data) {
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
