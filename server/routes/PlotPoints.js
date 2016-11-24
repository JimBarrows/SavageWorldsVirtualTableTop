var express         = require('express');
var passport        = require('passport');
var isAuthenticated = require('../authentication');
var router          = express.Router();
import PlotPoint from "../models/PlotPoint";

router.get('/', isAuthenticated, function (req, res) {
	PlotPoint.find({creator: req.user._id})
			.then((list) => res.status(200).json(list).end())
			.catch((error) => {
				console.log("Error retrieving list for ", user._id, " because", error);
				res.status(400).end();
			});

});

router.post('/', isAuthenticated, function (req, res) {
	let plotPoint     = req.body;
	plotPoint.creator = req.user._id;
	PlotPoint.create(plotPoint)
			.then((plotPoint) => res.status(201).json(plotPoint).end())
			.catch((error) => {
				console.log("Error saving plot point: ", error);
				res.status(400).end();
			});
});

router.put('/:plotPointId', isAuthenticated, function (req, res) {
	let plotPoint     = req.body;
	let {plotPointId} = req.params;
	plotPoint.creator = req.user._id;
	PlotPoint.findOneAndUpdate({_id: plotPointId}, plotPoint)
			.exec()
			.then((updated) => res.status(200).end())
			.catch((error) => {
				console.log("Error updating channel ", potPointId, " with error ", error);
				res.status(400).end();
			});
});

router.delete('/:plotPointId', isAuthenticated, function (req, res) {
	let {plotPointId} = req.params;
	PlotPoint.remove({_id: plotPointId})
			.then(() => res.status(200).end())
			.catch((error) => {
				console.log("Couldn't delete content ", contentId, " becuase ", error);
				res.status(400).end();
			})
});

router.get('/ping', function (req, res) {
	res.status(200).send("pong!");
});


module.exports = router;
