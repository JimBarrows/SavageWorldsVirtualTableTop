"use strict";
import mongoose from "mongoose";
import {level} from "./enums";
import Range from "./Range";

const Schema = mongoose.Schema,
      Types  = mongoose.Schema.Types;


const Power = new Schema({
	description: Types.String,
	duration: Types.Number,
	level: {type: Types.String, enum: level},
	maintenance: Types.Number,
	name: Types.String,
	powerPoints: Types.Number,
	range: Range,
	trappings: Types.String
});

export default Power;