"use strict";

module.exports = function(Sequelize, DataType) {
	var Skill = Sequelize.define('Skill', {
		rating: {
			type: DataType.INTEGER,
			allowNull: false,
			validate: {
				notEmpty: true,
			}
		},
		bonus: {
			type: DataType.INTEGER,
			allowNull: false,
			validate: {
				notEmpty: true
			}
		}
	}, {
		classMethods: {
			associate: function(models) {
				Skill.belongsTo(models.SkillDescription);
			}
		}
	});
	return Skill;
}