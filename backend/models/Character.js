"use strict";

module.exports = function(Sequelize, DataType) {
	
	var dieType = ["d4", "d6", "d8", "d10", "d12"];
	var levels = ['Novice','Seasoned', 'Veteran', 'Heroic', 'Legendary'];
	var  Character = Sequelize.define('Character', {
		name: {
			type: DataType.STRING,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		profession: {
			type: DataType.STRING,
			allowNull: true
		},
		description: {
			type: DataType.STRING,
			allowNull: true
		},
		agility:{
			type:DataType.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		smarts:{
			type:DataType.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		strength:{
			type:DataType.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		spirit:{
			type:DataType.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		vigor:{
			type:DataType.ENUM,
			values: dieType,
			allowNull:false,
			validate:{
				notEmpty: true
			}

		},
		experiencePoints:{
			type:DataType.INTEGER,
			defaultValue:0
		},
		level:{
			type:DataType.ENUM,
			values: levels,
		}
	}, {
		classMethods: {
			associate: function( models) {
				Character.belongsTo(		models.PlotPoint);
				Character.belongsToMany(	models.Edge, {
					through: 'characters_edges',
					onDelete: "CASCADE"});
				Character.belongsToMany(	models.Scene, {
					through: "scene_character",
					onDelete: "CASCADE"});
				Character.hasMany(			models.Gear);
				Character.hasMany(			models.Hindrance);
				Character.hasMany(			models.Skill,{
					onDelete: "CASCADE"
				});
			}
		}

	});
	return Character;
}
