"use strict";

module.exports = function(Sequelize, DataType) {
	var StandardGear = Sequelize.define('StandardGear', {
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
			allowNull:false,
			validate: {
				notEmpty: true
			}
		},
		weight: {
			type: DataType.INTEGER,
			allowNull:false,
			validate: {
				isNumeric: true,
				isInt: true, 
			}
		},
		cost: {
			type: DataType.INTEGER,
			allowNull: false,
			validate: {
				isNumeric: true,
				isInt: true
			}
		},
		subType: {
			type: DataType.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			}
		},
		notes: {
			type: DataType.STRING,
			allowNull:false,
			validate: {
				notEmpty: true
			}
		}
	});
	return StandardGear;
};