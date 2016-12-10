"use strict";
import mongoose from "mongoose";
const Schema = mongoose.Schema,
      Types  = mongoose.Schema.Types;


const Range = new Schema({
	short: Types.Number,
	medium: Types.Number,
	long: Types.Number
});

export default Range;