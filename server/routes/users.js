var express  = require('express');
var passport = require('passport');
var Account = require('../models/Account');
var router   = express.Router();

/* GET users listing. */
router.get('/', function (req, res) {
	res.send('respond with a resource. Foo.');
});

router.post('/register', function (req, res) {
	Account.register(new Account({username: req.body.username}), req.body.password, function (err, account) {
		if (err) {
			return res.json({error: err.message});
		}

		passport.authenticate('local')(req, res, function () {
			req.session.save(function (err) {
				if (err) {
					return next(err);
				}
				res.json({id: account._id, username: account.username});
			});
		});
	});
});

router.post('/login', passport.authenticate('local'), function (req, res) {
	res.json({
		username: req.user.username
		, id: req.user._id
	})
});

router.get('/logout', function (req, res) {
	req.logout();
	res.status(200).end();
});

module.exports = router;
