"use strict";

module.exports = function(Sequelize, DataType) {
	var StandardSkill = Sequelize.define('StandardSkill', {
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
		attribute: {
			type: DataType.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			},
			values: ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor']
		}
	});
	return StandardSkill;
}