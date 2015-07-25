module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var standardHindrance = db.define('StandardHindrance', {
  		name: {
    		type: sequelize.STRING,
    		allowNull: false,
    		validate: {
    			notEmpty: true,
    		}
  		},
		severity: {
    		type: sequelize.ENUM,
    		values: ['Major', 'Minor', 'Major or Minor'],
    		allowNull: false,
    		validate: {
    			notEmpty: true
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
		standardHindrance.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'standardHindrance': data
			});
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.standardHindrance;
		standardHindrance.create(newRec).then( function(data) {
			res.status(201).send({ standardHindrance: data}).end();	
		});
	});

	router.get('/:id', function(req, res) {
		standardHindrance.findById( req.params.id).then(function(data){
			res.send({
				'standardHindrance':data
			});	
		})
		
	});

	router.put('/:id', function(req, res) {
		standardHindrance.findById( req.params.id).then(function(data){
			data.updateAttributes(req.body.standardHindrance).then(function(data) {
				res.send({
					'standardHindrance': data
				});
			});
		});
	});

	router.delete('/:id', function(req, res) {
		standardHindrance.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		});
	});

	return router;
}
