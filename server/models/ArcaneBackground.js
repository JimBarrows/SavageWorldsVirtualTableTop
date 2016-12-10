"use strict";
import mongoose from "mongoose";
import SkillDescription from "./SkillDescription";

const Schema = mongoose.Schema,
      Types  = mongoose.Schema.Types;


const ArcaneBackground = new Schema({
	name: Types.String,
	skill: SkillDescription,
	startingPowerPoints: Types.Number,
	startingPowers: Types.Number,
	description: Types.String
});

export default ArcaneBackground;