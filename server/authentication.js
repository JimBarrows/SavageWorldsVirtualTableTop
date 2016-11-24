import Account from "./models/Account";
import config from "./config";
import jwt from "jsonwebtoken";

export default function (req, res, next) {
	var token = req.body.token || req.query.token || req.headers[config.jwt.header];
	if (token) {
		jwt.verify(token, config.jwt.secret, function (err, decoded) {
			if (err) {
				res.status(403).end();
			} else {
				Account.findById(decoded._doc._id)
						.exec()
						.then((account) => {
							if (account) {
								req.user    = account;
								req.decoded = decoded;
								next();
							} else {
								res.status(403).end();
							}
						})
						.catch((error) => {
							console.log("error: ", error);
							res.status(403).end();
						});
			}
		});
	} else {
		res.status(403).end();
	}
}