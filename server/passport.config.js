import passport from "passport";
import {Strategy as LocalStrategy} from "passport-local";
import Account from "./models/Account";

passport.use(new LocalStrategy(Account.authenticate()));

passport.serializeUser(Account.serializeUser());
passport.deserializeUser(Account.deserializeUser());

export default passport;