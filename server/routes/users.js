import Account from "../models/Account";
import config from "../config";
import express from "express";
import jwt from "jsonwebtoken";
import passport from "passport";

const router     = express.Router();
const emailRegex = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;

router.post("/", function (req, res) {
	let {username, password} = req.body;
	if (!emailRegex.test(username)) {
		console.log(`User registration failed with username: ${username} because username is not a valid email address.`);
		res.status(400).json({error: {"username": "Username must be an email"}});
		return;
	}
	Account.register(
			new Account({username}),
			password,
			function (err, user) {
				if (err) {
					console.log(`User registration failed with username: ${username} because ${err}`);
					res.status(400).json({error: {"username": err.message}}).end();
				} else {
					var token = jwt.sign(user, config.jwt.secret);
					res.status(200).json({token: token});
				}
			});
});

router.post('/authenticate', passport.authenticate('local'), function (req, res) {
	var token = jwt.sign(req.user, config.jwt.secret);
	res.status(200).json({token: token});
});


export default router;