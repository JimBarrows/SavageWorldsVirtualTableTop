import express from "express";
import isAuthenticated from "../authentication";
import PlotPoint from "../models/PlotPoint";
import {plotPointIsValid} from "./validators";

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
	if (plotPointIsValid(plotPoint)) {
		PlotPoint.create(plotPoint)
				.then((plotPoint) => res.status(201).json(plotPoint).end())
				.catch((error) => {
					res.status(400).json({error}).end();
				});
	} else {
		res.status(400).json({"error": {"validation": {"name": "is required"}}}).end();
	}

});

export default router;
