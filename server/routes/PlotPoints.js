import express from "express";
import isAuthenticated from "../authentication";
import PlotPoint from "../models/PlotPoint";

const router = express.Router();

router.get("/", isAuthenticated, function (req, res) {
	PlotPoint.find({user: req.user._id})
			.then((list) => res.status(200).json(list).end())
			.catch((error) => {
				res.status(400).json({error}).end();
			});

});

router.post("/", isAuthenticated, function (req, res) {
	let plotPoint  = req.body;
	plotPoint.user = req.user._id;
	if (plotPoint.name) {
		PlotPoint.create(plotPoint)
				.then((plotPoint) => res.status(201).json(plotPoint).end())
				.catch((error) => {
					res.status(400).json({error}).end();
				});
	} else {
		res.status(400).json({"error": {"validation": {"name": "is required"}}}).end();
	}

});

router.put("/:plotPointId", isAuthenticated, function (req, res) {
	let plotPoint     = req.body;
	let {plotPointId} = req.params;
	plotPoint.user = req.user._id;
	PlotPoint.findOneAndUpdate({_id: plotPointId}, plotPoint)
			.exec()
			.then((updated) => res.status(200).end())
			.catch((error) => {
				res.status(400).end();
			});
});

router.delete("/:plotPointId", isAuthenticated, function (req, res) {
	let {plotPointId} = req.params;
	PlotPoint.remove({_id: plotPointId})
			.then(() => res.status(200).end())
			.catch((error) => {
				console.log("Couldn't delete content ", contentId, " becase ", error);
				res.status(400).end();
			})
});

export default router;
