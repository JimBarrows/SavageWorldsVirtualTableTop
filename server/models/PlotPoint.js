'use strict';
import mongoose from "mongoose";
const Schema = mongoose.Schema;
const Types  = mongoose.Schema.Types;

export const ranks = ['d4', 'd6', 'd8', 'd10', 'd12'];

export const attributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];

const Ammo = new Schema({
	name: Types.String,
	weightNumerator: Types.Number,
	weightDenominator: Types.Number,
	costNumerator: Types.Number,
	costDenominator: Types.Number,
	notes: Types.String
});


const Armor = new Schema({
	name: Types.String,
	type: Types.String,
	era: Types.String,
	points: Types.Number,
	pointsVsBullets: Types.Number,
	armorProtection: Types.Number,
	apVsBullets: Types.Number
});

const AttributeRank = new Schema({
	attribute: {type: Types.String, enum: attributes},
	rank: {type: Types.String, enum: ranks}
});

const EdgeType = new Schema({
	name: Types.String,
	description: Types.String
});

const SkillRank = new Schema({
	description: [Types.ObjectId],
	rank: {type: Types.String, enum: ranks}
});

const EdgeDescription = new Schema({
	name: Types.String,
	description: Types.String,
	effects: Types.String,
	edgeRequirements: [Types.ObjectId],
	attributeRankRequirements: [AttributeRank],
	skillRankRequirements: [SkillRank],
	edgeType: EdgeType
});

const HandWeapon = new Schema({
	name: Types.String,
	type: Types.String,
	era: Types.String,
	weight: Types.Number,
	cost: Types.Number,
	ability: {type: Types.String, enum: attributes},
	dice: {type: Types.String, enum: ranks},
	bonus: Types.Number,
	notes: Types.String
});

const HindranceDescription = new Schema({
	name: Types.String,
	description: Types.String,
	effects: Types.String,
	severity: Types.String
});

const MundaneItem = new Schema({
	name: Types.String,
	weight: Types.Number,
	cost: Types.Number,
	type: Types.String
});

const RaceAbility = new Schema({
	name: Types.String,
	description: Types.String,
	cost: Types.Number
});

const Race = new Schema({
	name: Types.String,
	description: Types.String,
	abilities: [RaceAbility]
});

const RangedWeapon = new Schema({
	name: Types.String,
	type: Types.String,
	era: Types.String,
	weight: Types.Number,
	cost: Types.Number,
	ability: {type: Types.String, enum: attributes},
	dice: {type: Types.String, enum: ranks},
	bonus: Types.Number,
	rateOfFire: Types.Number,
	shots: Types.Number,
	minStr: {Type: Types.String, enum: ranks},
	notes: Types.String,
	short: Types.Number,
	medium: Types.Number,
	long: Types.Number
});

const SettingRule = new Schema({
	name: Types.String,
	description: Types.String
});

const SkillDescription = new Schema({
	name: Types.String,
	description: Types.String,
	attribute: Types.String
});

const SpecialWeapon = new Schema({
	name: Types.String,
	type: Types.String,
	era: Types.String,
	short: Types.Number,
	medium: Types.Number,
	long: Types.Number,
	numberOfDie: Types.Number,
	dice: {type: Types.String, enum: ranks},
	bonus: Types.Number,
	rateOfFire: Types.Number,
	minStrength: {type: Types.String, enum: ranks},
	burstTemplate: Types.String,
	weight: Types.Number,
});

const VehicleMountedAndAtGun = new Schema({
	name: Types.String,
	type: Types.String,
	era: Types.String,
	short: Types.Number,
	medium: Types.Number,
	long: Types.Number,
	numberOfDice: Types.Number,
	dice: {type: Types.String, enum: ranks},
	armorPiercing: Types.Number,
	heNumberOfDice: Types.Number,
	heDice: {type: Types.String, enum: ranks},
	heAp: Types.Number,
	burstTemplate: Types.String,
	rateOfFire: Types.Number
});

const PlotPoint = new Schema({
	ammunition: [Ammo],
	armor: [Armor],
	description: Types.String,
	edges: [EdgeDescription],
	edgeTypes: [EdgeType],
	handWeapons: [HandWeapon],
	hindrances: [HindranceDescription],
	mundaneItems: [MundaneItem],
	name: Types.String,
	races: [Race],
	rangedWeapons: [RangedWeapon],
	settingRules: [SettingRule],
	skillDescriptions: [SkillDescription],
	specialWeapons: [SpecialWeapon],
	startingFund: Types.Number,
	user: Types.ObjectId,
	vehicleMountedAndAtGuns: [VehicleMountedAndAtGun]
});

export default  mongoose.model('PlotPoint', PlotPoint);