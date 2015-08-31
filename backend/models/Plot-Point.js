"use strict";

module.exports = function(Sequelize, DataType) {
	var PlotPoint = Sequelize.define('PlotPoint', {
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
		bloodAndGuts: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		bornAHero: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		criticalFailures: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		fantatics: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		grittyDamage: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		heroesNeverDie: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		highAdventure: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		jokersWild: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		multipleLanguages: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		noPowerPoints: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		skillSpecialization: {
			type: DataType.BOOLEAN,
			defaultValue: false
		},
		startingAttributePoints: {
			type: DataType.INTEGER,
			defaultValue: 5
		},
		startingSkillPoints: {
			type: DataType.INTEGER,
			defaultValue: 15
		},
		startingMajorHindrances: {
			type: DataType.INTEGER,
			defaultValue: 1
		},
		startingMinorHindrances: {
			type: DataType.INTEGER,
			defaultValue: 2
		},
		startingCash: {
			type: DataType.INTEGER,
			defaultValue: 500
		}
	}, {
		classMethods: {
			associate: function(models) {
				PlotPoint.hasMany(         models.Character, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
				PlotPoint.hasMany(         models.Edge, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
				PlotPoint.hasMany(         models.Gear, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
				PlotPoint.hasMany(         models.Hindrance, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
				PlotPoint.hasMany(         models.Power, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
				PlotPoint.hasMany(         models.Race, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
				PlotPoint.hasMany(         models.SkillDescription, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
				PlotPoint.hasMany(         models.Story, {
					onDelete: "CASCADE",
					foreignKey: {
						allowNull: false
					}
				});
			}
		}
	});
	return PlotPoint;
}
