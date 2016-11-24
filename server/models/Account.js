import mongoose from "mongoose";
import PassportLocalMongoose from "passport-local-mongoose";

const Schema = mongoose.Schema,
      Types  = mongoose.Schema.Types;

const Account = new Schema({});

Account.plugin(PassportLocalMongoose);

export default mongoose.model('Account', Account);