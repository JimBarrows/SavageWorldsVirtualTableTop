module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var standardPower = db.define('StandardPower', {
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
		},
		rank: {
			type: sequelize.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			},
			values: ['Novice', 'Seasoned', 'Veteran', 'Heroic', 'Legendary']
		},
		powerPoints: {
			type: sequelize.INTEGER,
			allowNull:false,
			validate:{
				isNumeric: true,
				isInt: true,
				min: 0  
			}
		},
		range: {
			type: sequelize.STRING,
		},
		duration: {
			type: sequelize.INTEGER,
			allowNull:false,
			validate:{
				isNumeric: true,
				isInt: true,
				min: 0  
			}
		},
		maintenanceCost: {
			type: sequelize.INTEGER,
			allowNull:false,
			validate:{
				isNumeric: true,
				isInt: true,
				min: 0  
			}
		},
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	router.get('/', function(req, res) {
		standardPower.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'standardPower': data
			});
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.standardPower;
		standardPower.create(newRec)
			.then( function(data) {
				res.status(201).send({ standardPower: data}).end();	
			})
			.catch( function(error){
				res.status(400, error);
			});
	});

	router.get('/:id', function(req, res) {
		standardPower.findById( req.params.id).then(function(data){
			res.send({
				'standardPower':data
			});	
		})
		
	});

	router.put('/:id', function(req, res) {
		standardPower.findById( req.params.id).then(function(data){
			data.updateAttributes(req.body.standardPower).then(function(data) {
				res.send({
					'standardPower': data
				});
			});
		});
	});

	router.delete('/:id', function(req, res) {
		standardPower.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		});
	});

	return router;
}
