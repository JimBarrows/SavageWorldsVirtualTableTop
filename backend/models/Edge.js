"use strict";

module.exports = function(Sequelize, DataType) {
	
	var Edge = Sequelize.define('Edge',{
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
				Edge.belongsToMany(models.Character, {
					through: 'characters_edges'
				});
			}
		}
	});
	return Edge;
}