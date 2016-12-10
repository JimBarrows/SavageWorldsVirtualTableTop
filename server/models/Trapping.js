"use strict";
import mongoose from "mongoose";
const Schema = mongoose.Schema,
      Types  = mongoose.Schema.Types;


const Trapping = new Schema({
	name: Types.String,
	description: Types.String,
	type: Types.String
});

export default Trapping;