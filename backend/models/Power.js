"use strict";

module.exports = function(Sequelize, DataType) {
	var Power = Sequelize.define('Power', {
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
		powerPoints: {
			type: DataType.INTEGER,
			allowNull: false,
			defaultValue: 1,
			validate: {
				min: 1
			}
		},
		rank:{
			type: DataType.ENUM,
			values: ['Novice','Seasoned', 'Veteran', 'Heroic', 'Legendary'],
			allowNull: false,
			defaultValue: 'Novice'
		},
		duration:{
			type: DataType.STRING,
			defaultValue: 'Instant'
		},
		maintenanceCost:{
			type: DataType.STRING,
			allowNull: true
		}

	}, {
		classMethods: {
			associate: function(models) {
				Power.belongsTo(           models.PlotPoint);
			}
		}
	});
	return Power;
}