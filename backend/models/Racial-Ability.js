"use strict";

module.exports = function(Sequelize, DataType) {
	var RacialAbility = Sequelize.define('RacialAbility',{
		description: {
			type: DataType.STRING,
			allowNull: false,
			validate: {
				notEmpty: true
			}
		},
		cost: {
			type: DataType.INTEGER,
			defaultValue: 1,
			validate: {
				min: -3,
				max: 3
			}
		}
	}, {
		classMethods: {
			associate: function(models) {
				RacialAbility.belongsTo(   models.Race);
			}
		}
	});
	return RacialAbility;
}