module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var standardGear = db.define('StandardGear', {
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
		era: {
			type: sequelize.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			}
		},
		weight: {
			type: sequelize.INTEGER,
			allowNull:false,
			validate: {
				isNumeric: true,
				isInt: true, 
			}
		},
		cost: {
			type: sequelize.INTEGER,
			allowNull: false,
			validate: {
				isNumeric: true,
				isInt: true
			}
		},
		subType: {
			type: sequelize.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			}
		},
		notes: {
			type: sequelize.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			}
		}
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	router.get('/', function(req, res) {
		standardGear.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'standardGear': data
			});
		}).catch( function(error){
			console.log("error: " + error);
			res.status(400, error).end();
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.standardGear;
		standardGear.create(newRec)
		.then( function(data) {
			console.log("before");
			res.status(201).send({ standardGear: data}).end();	
			console.log("after");
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400, error).end();
		});
	});

	router.get('/:id', function(req, res) {
		standardGear.findById( req.params.id).then(function(data){
			res.send({
				'standardGear':data
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400, error).end();
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
			console.log("error: " + error);
			res.status(400, error).end();
		});
	});

	router.delete('/:id', function(req, res) {
		standardGear.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400, error).end();
		});
	});

	return router;
}
