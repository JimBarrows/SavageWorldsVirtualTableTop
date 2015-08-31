"use strict";

module.exports = function(Sequelize, DataType) {
	var StandardPowerTrapping = Sequelize.define('StandardPowerTrapping', {
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
		type: {
			type: DataType.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			},
			values: ['Acid', 'Cold/Ice', 'Darkness', 'Electricity', 'Fire/Heat', 'Light', 'Necromantic', 'Sound']
		}
	});
	return StandardPowerTrapping;
}