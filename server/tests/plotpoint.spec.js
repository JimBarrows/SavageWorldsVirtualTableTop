import {client, expect, plotPoint as createPlotPoint, startTestHouseKeeping} from "./support/fixtures";
import PlotPoint from "../models/PlotPoint";

describe("/api/plotpoint/:plotPointId", function () {

	let url      = "/api/plotpoint";
	let currentUser = {};
	let original = {};
	let updated  = {};

	beforeEach(() => startTestHouseKeeping()
			.then((account) => currentUser = account)
			.then(() => PlotPoint.create(createPlotPoint("original plot point", currentUser._id)))
			.then((created) => original = created));

	describe("PUT method", function () {

		it("should update a plot point", function () {
			updated = Object.assign(original, {name: "this is a new name"});
			return client.put(`${url}/${updated._id}`, updated)
					.then((response) => {
						expect(response.status).to.be.equal(200);
						return PlotPoint.findById(updated._id).exec();
					})
					.then((data) => {
						expect(data.name).to.be.equal(updated.name);
					});
		});

		it("should reject a plot point update with no name", function () {
			updated = Object.assign(original, {name: ""});
			return client.put(`${url}/${updated._id}`, updated)
					.then((response) => {
						expect(response.status).to.be.equal(400);

					});
		});
	});
	// describe("");
});