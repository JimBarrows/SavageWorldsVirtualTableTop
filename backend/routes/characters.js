module.exports = function(db) {
	var express = require('express');
	var router = express.Router();
	var sequelize = require('sequelize');

	var dieType = ["d4", "d6", "d8", "d10", "d12"];
	var levels = ['Novice','Seasoned', 'Veteran', 'Heroic', 'Legendary']

	var PlotPoint = db.models.PlotPoint;
	var Edge = db.models.Edge;
	var Skill = db.models.Skill;

	var Character = db.define('Character', {
		name: {
			type: sequelize.STRING,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		profession: {
			type: sequelize.STRING,
			allowNull: true
		},
		description: {
			type: sequelize.STRING,
			allowNull: true
		},
		agility:{
			type:sequelize.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		smarts:{
			type:sequelize.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		strength:{
			type:sequelize.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		spirit:{
			type:sequelize.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		vigor:{
			type:sequelize.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		experiencePoints:{
			type:sequelize.INTEGER,
			defaultValue:0
		},
		level:{
			type:sequelize.ENUM,
			values: levels,
		}
	}, {
		freezeTableName: true // Model tableName will be the same as the model name
	});

	Character.belongsTo( PlotPoint);
	Character.belongsToMany( Edge, {through: 'characters_edges'});
	Edge.belongsToMany( Character, {through: 'characters_edges'});
	Character.hasMany( Skill);

	router.get('/', function(req, res) {
		Character.findAll({order: 'name ASC'}).then(function(data) {
			res.send({
				'character': data
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	router.post('/', function(req, res) {
		var newRec = req.body.character;
		Character.create(newRec)
			.then( function(data) {
				res.status(201).send({ character: data}).end();	
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.get('/:id', function(req, res) {
		Character.findById( req.params.id).then(function(data){
			res.send({
				'character':data
			});	
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {error: error}).end();
		});
	});

	router.put('/:id', function(req, res) {
		Character.findById( req.params.id)
			.then(function(data){
				data.updateAttributes(req.body.character)
					.then(function(data) {
						res.send({
							'character': data
						});
					});
			})
			.catch( function(error){
				console.log("error: " + error);
				res.status(400).send( {"errors": error}).end();
			});
	});

	router.delete('/:id', function(req, res) {
		Character.findById( req.params.id).then(function(data) {
			data.destroy().then(function(){
				res.status(204).end();	
			});
		})
		.catch( function(error){
			console.log("error: " + error);
			res.status(400).send( {"errors": error}).end();
		});
	});

	return router;
}
