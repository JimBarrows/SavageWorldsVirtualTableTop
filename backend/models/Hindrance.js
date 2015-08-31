"use strict";

module.exports = function(Sequelize, DataType) {
	var Hindrance = Sequelize.define('Hindrance', {
		name: {
			type: DataType.STRING,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		severity: {
			type: DataType.ENUM,
			values: ['Major', 'Minor', 'Major or Minor'],
			allowNull: false,
			validate: {
				notEmpty: true
			}
		},
		description: {
			type: DataType.STRING,
			allowNull: false,
			validate: {
				notEmpty: true
			}
		}
	}, {
		classMethods: {
			associate: function(models) {
				Hindrance.belongsTo( models.Character);
			}
		}
	});
	return Hindrance;
}