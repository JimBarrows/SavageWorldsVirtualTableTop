import config from "./config";
import mongoose from "mongoose";

mongoose.Promise = require('bluebird');

mongoose.connect(config.mongoose.url, function (err) {
	if (err) {
		console.log("Could not connect to mongodb on localhost. Ensure that you have mongodb running on localhost and mongodb accepts connections on standard ports!");
	}
});

export default mongoose;
