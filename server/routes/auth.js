var express  = require('express');
var passport = require('passport');
var router   = express.Router();

router.get('/facebook',
		passport.authenticate('facebook'),
		function (req, res) {
		});
router.get('/auth/facebook/callback',
		passport.authenticate('facebook', {failureRedirect: '/'}),
		function (req, res) {
			res.redirect('/account');
		});

module.exports = router;