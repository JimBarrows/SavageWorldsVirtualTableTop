"use strict";

module.exports = function(Sequelize, DataType) {
	var Scene = Sequelize.define('Scene', {
		name: {
			type: DataType.STRING,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		description: {
			type: DataType.STRING,
			allowNull: true
		}
	}, {
		classMethods: {
			associate: function(models) {
				Scene.belongsToMany(       models.Character, {through: 'scene_character'});
			}
		}
	});
	return Scene;
}