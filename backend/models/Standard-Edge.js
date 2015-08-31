"use strict";

module.exports = function(Sequelize, DataType) {
	var StandardEdge = Sequelize.define('StandardEdge', {
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
		}
	});
	return StandardEdge;
}