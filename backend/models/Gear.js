"use strict";

module.exports = function(Sequelize, DataType) {
	var Gear = Sequelize.define('Gear', {
		name: {
			type: DataType.STRING,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		description: {
			type: DataType.STRING,
			allowNull: false,
			validate: {
				notEmpty: true
			}
		},
		era: {
			type: DataType.STRING,
			allowNull: false
		},
		weight:{
			type: DataType.INTEGER,
			allowNull: false,
			defaultValue: 1
		},
		cost:{
			type: DataType.INTEGER,
			defaultValue: 1
		},
		subType:{
			type: DataType.STRING,
			allowNull: false
		},
		notes:{
			type: DataType.STRING,
			allowNull: false
		}

	}, {
		classMethods: {
			associate: function(models) {
				Gear.belongsTo(models.Character, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
			}
		}
	});
	return Gear;
}