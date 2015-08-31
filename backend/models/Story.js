"use strict";

module.exports = function(Sequelize, DataType) {
	var Story = Sequelize.define('Story', {
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
				Story.hasMany(models.Chapter, {
					ondDelete: "CASCADE"
				});
				Story.belongsTo(models.PlotPoint);
			}
		}
	});
	return Story;
}