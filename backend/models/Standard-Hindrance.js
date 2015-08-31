"use strict";

module.exports = function(Sequelize, DataType) {
	var StandardHindrance = Sequelize.define('StandardHindrance', {
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
		freezeTableName: true // Model tableName will be the same as the model name
	});
	return StandardHindrance;
}