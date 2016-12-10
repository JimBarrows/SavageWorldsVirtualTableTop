'use strict';
import mongoose from "mongoose";
import ArcaneBackground from "./ArcaneBackground";
import SkillDescription from "./SkillDescription";
import Trapping from "./Trapping";
import {ranks, attributes, hindranceSeverity} from "./enums";
const Schema = mongoose.Schema;
const Types  = mongoose.Schema.Types;


const Ammo = new Schema({
	name: Types.String,
	cost: Types.Number,
	weight: Types.Number,
	notes: Types.String
});

const Armor = new Schema({
	name: Types.String,
	type: Types.String,
	era: Types.String,
	armor: Types.Number,
	armorVsBullets: Types.Number,
	armorProtection: Types.Number,
	cost: Types.Number,
	weight: Types.Number
});

const AttributeRank = new Schema({
	attribute: {type: Types.String, enum: attributes},
	rank: {type: Types.String, enum: ranks}
});

const Damage = new Schema({
	attribute: {type: Types.String, enum: attributes},
	diceCount: {type: Types.Number},
	bonus: Types.Number,
	dice: {type: Types.String, enum: ranks}
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
	edgeRequirements: [Types.ObjectId],
	attributeRankRequirements: [AttributeRank],
	skillRankRequirements: [SkillRank],
	edgeType: EdgeType
});

const HandWeapon = new Schema({
	cost: Types.Number,
	damage: Damage,
	era: Types.String,
	name: Types.String,
	notes: Types.String,
	type: Types.String,
	weight: Types.Number
});

const HindranceDescription = new Schema({
	name: Types.String,
	description: {type: Types.String, enum: hindranceSeverity},
	severity: Types.String
});

const MundaneItem = new Schema({
	cost: Types.Number,
	name: Types.String,
	type: Types.String,
	weight: Types.Number
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

const Range = new Schema({
	short: Types.Number,
	medium: Types.Number,
	long: Types.Number
});

const RangedWeapon = new Schema({
	cost: Types.Number,
	damage: Damage,
	era: Types.String,
	minStr: {Type: Types.String, enum: ranks},
	name: Types.String,
	notes: Types.String,
	range: Range,
	rateOfFire: Types.Number,
	shots: Types.Number,
	type: Types.String,
	weight: Types.Number
});

const SettingRule = new Schema({
	name: Types.String,
	description: Types.String
});

const SpecialWeapon = new Schema({
	name: Types.String,
	type: Types.String,
	range: Range,
	damage: Damage,
	rateOfFire: Types.Number,
	armorPiercing: Types.Number,
	minStrength: {type: Types.String, enum: ranks},
	burstTemplate: Types.String,
	weight: Types.Number,
});

const VehicleMountedAndAtGun = new Schema({
	name: Types.String,
	type: Types.String,
	era: Types.String,
	range: Range,
	apRoundDamage: Damage,
	heRoundDamage: Damage,
	rateOfFire: Types.Number
});

const Vehicle = new Schema({
	era: Types.String,
	type: Types.String,
	mode: Types.String,
	name: Types.String,
	acceleration: Types.Number,
	topSpeed: Types.Number,
	toughness: Types.Number,
	armorPoints: Types.Number,
	crew: Types.Number,
	passengers: Types.Number,
	cost: Types.Number,
	notes: Types.String,
	examples: Types.String,
	weapons: Types.String,
	climb: Types.Number
});

const PlotPoint = new Schema({
	ammunition: [Ammo],
	armor: [Armor],
	arcaneBackgrounds: [ArcaneBackground],
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
	trappings: [Trapping],
	user: Types.ObjectId,
	vehicleMountedAndAtGuns: [VehicleMountedAndAtGun],
	vehicles: [Vehicle]
});

export default  mongoose.model('PlotPoint', PlotPoint);