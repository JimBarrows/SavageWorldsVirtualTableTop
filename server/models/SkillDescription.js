"use strict";
import mongoose from "mongoose";
import {attributes} from "./enums";
const Schema = mongoose.Schema,
      Types  = mongoose.Schema.Types;

const SkillDescription = new Schema({
	name: Types.String,
	description: Types.String,
	attribute: {type: Types.String, enum: attributes}
});

export default SkillDescription;