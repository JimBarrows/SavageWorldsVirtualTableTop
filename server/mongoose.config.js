import mongoose from "mongoose";

mongoose.Promise = require('bluebird');

mongoose.connect('mongodb://localhost/swvtt');

module.exports = mongoose;
