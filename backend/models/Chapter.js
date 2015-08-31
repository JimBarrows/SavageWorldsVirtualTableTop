"use strict";

module.exports = function(Sequelize, DataType) {
	var Chapter = Sequelize.define('Chapter', {
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
				Chapter.hasMany(models.Scene, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
			}
		}
	});
	return Chapter;
}