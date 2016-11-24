var passport         = require('passport');
var LocalStrategy    = require('passport-local').Strategy;
var FacebookStrategy = require('passport-facebook').Strategy;
var Account          = require('./models/account');
var ids              = require('./external_logins');

passport.use(new LocalStrategy(Account.authenticate()));

// passport.use(new FacebookStrategy({
// 			clientID: ids.facebook.clientID,
// 			clientSecret: ids.facebook.clientSecret,
// 			callbackURL: ids.facebook.callbackURL
// 		},
// 		function (accessToken, refreshToken, profile, done) {
// 			Account.findOne({oauthID: profile.id}, function (err, user) {
// 				if (err) {
// 					console.log(err);  // handle errors!
// 				}
// 				if (!err && user !== null) {
// 					done(null, user);
// 				} else {
// 					user = new Account({
// 						oauthID: profile.id,
// 						name: profile.displayName,
// 						created: Date.now()
// 					});
// 					user.save(function (err) {
// 						if (err) {
// 							console.log(err);  // handle errors!
// 						} else {
// 							console.log("saving user ...");
// 							done(null, user);
// 						}
// 					});
// 				}
// 			});
// 		}
// ));
passport.serializeUser(Account.serializeUser());
passport.deserializeUser(Account.deserializeUser());

module.exports = passport;