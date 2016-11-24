var express         = require('express');
var passport        = require('passport');
var isAuthenticated = require('../authentication');
var router          = express.Router();

/* GET home page. */
router.get('/', function (req, res) {
	res.render('index', {title: 'Express'});
});

router.get('/protected', isAuthenticated, function (req, res) {
	res.render('protected');
});


router.get('/ping', function (req, res) {
	res.status(200).send("pong!");
});


module.exports = router;
