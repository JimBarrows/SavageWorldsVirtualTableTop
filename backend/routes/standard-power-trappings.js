module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var standardPowerTrapping = db.define('StandardPowerTrapping', {
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
		type: {
			type: sequelize.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			},
			values: ['Acid', 'Cold/Ice', 'Darkness', 'Electricity', 'Fire/Heat', 'Light', 'Necromantic', 'Sound']
		}
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	router.get('/', function(req, res) {
		standardPowerTrapping.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'standardPowerTrapping': data
			});
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.standardPowerTrapping;
		console.log("name: " + newRec.name);
		standardPowerTrapping.create(newRec)
			.then( function(data) {
				console.log("before");
				res.status(201).send({ standardPowerTrapping: data}).end();	
				console.log("after");
			})
			.catch( function(error){
				res.status(400, error);
			});
	});

	router.get('/:id', function(req, res) {
		standardPowerTrapping.findById( req.params.id).then(function(data){
			res.send({
				'standardPowerTrapping':data
			});	
		})
		
	});

	router.put('/:id', function(req, res) {
		standardPowerTrapping.findById( req.params.id).then(function(data){
			data.updateAttributes(req.body.standardPowerTrapping).then(function(data) {
				res.send({
					'standardPowerTrapping': data
				});
			});
		});
	});

	router.delete('/:id', function(req, res) {
		standardPowerTrapping.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		});
	});

	return router;
}
