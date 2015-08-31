"use strict";

module.exports = function(Sequelize, DataType) {
	var StandardPower = Sequelize.define('StandardPower', {
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
		rank: {
			type: DataType.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			},
			values: ['Novice', 'Seasoned', 'Veteran', 'Heroic', 'Legendary']
		},
		powerPoints: {
			type: DataType.INTEGER,
			allowNull:false,
			validate:{
				isNumeric: true,
				isInt: true,
				min: 0  
			}
		},
		range: {
			type: DataType.STRING,
		},
		duration: {
			type: DataType.INTEGER,
			allowNull:false,
			validate:{
				isNumeric: true,
				isInt: true,
				min: 0  
			}
		},
		maintenanceCost: {
			type: DataType.INTEGER,
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
	return StandardPower;
}