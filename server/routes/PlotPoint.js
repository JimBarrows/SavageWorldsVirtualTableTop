import express from "express";
import isAuthenticated from "../authentication";
import PlotPoint from "../models/PlotPoint";
import {plotPointIsValid} from "./validators";

const router = express.Router();

router.put("/:plotPointId", isAuthenticated, function (req, res) {
	let plotPoint     = req.body;
	let {plotPointId} = req.params;
	plotPoint.user    = req.user._id;
	if (plotPointIsValid(plotPoint)) {
		PlotPoint.findOneAndUpdate({_id: plotPointId}, plotPoint)
				.exec()
				.then((updated) => res.status(200).end())
				.catch((error) => {
					res.status(400).json(error).end();
				});
	} else {
		res.status(400).json({"error": {"validation": {"name": "is required"}}}).end();
	}
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