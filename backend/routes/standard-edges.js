module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var standardEdge = db.define('StandardEdge', {
  		name: {
    		type: sequelize.STRING,
    		allowNull: false,
    		validate: {
    			notEmpty: true,
    		}
  		},
		description: {
    		type: sequelize.STRING,
    		allowNull: false,
    		validate: {
    			notEmpty: true
    		}
  		}
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	router.get('/', function(req, res) {
		standardEdge.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'standardEdge': data
			});
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.standardEdge;
		standardEdge.create(newRec).then( function(data) {
			res.status(201).send({ standardEdge: data}).end();	
		});
	});

	router.get('/:id', function(req, res) {
		standardEdge.findById( req.params.id).then(function(data){
			res.send({
				'standardEdge':data
			});	
		})
		
	});

	router.put('/:id', function(req, res) {
		standardEdge.findById( req.params.id).then(function(data){
			data.updateAttributes(req.body.standardEdge).then(function(data) {
				res.send({
					'standardEdge': data
				});
			});
		});
	});

	router.delete('/:id', function(req, res) {
		standardEdge.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		});
	});

	return router;
}
