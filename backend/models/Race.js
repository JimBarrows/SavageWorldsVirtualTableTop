"use strict";

module.exports = function(Sequelize, DataType) {
	var Race = Sequelize.define('Race', {
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
	}, {
		classMethods: {
			associate: function(models) {
				Race.hasMany( models.RacialAbility, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
			}
		}
	});
	
	return Race;
};