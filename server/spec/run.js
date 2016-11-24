/**
 * Created by JimBarrows on 7/4/16.
 */
import Jasmine from "jasmine";
import mongoose from "../mongoose.config.js";
mongoose;
var jasmine = new Jasmine();
jasmine.loadConfigFile('spec/support/jasmine.json');
jasmine.execute();

