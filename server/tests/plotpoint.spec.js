import {client, expect, plotPoint as createPlotPoint, startTestHouseKeeping} from "./support/fixtures";
import PlotPoint from "../models/PlotPoint";

describe("/api/plotpoint/:plotPointId", function () {

	let url      = "";
	let currentUser = {};
	let original = {};
	let updated  = {};

	beforeEach(() => startTestHouseKeeping()
			.then((account) => currentUser = account)
			.then(() => PlotPoint.create(createPlotPoint("original plot point", currentUser._id)))
			.then((created) => original = created)
			.then(() => url = `/api/plotpoint/${original._id}`));

	describe("PUT method", function () {

		it("should update a plot point", function () {
			updated = Object.assign(original, {name: "this is a new name"});
			return client.put(url, updated)
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
			return client.put(url, updated)
					.then((response) => {
						expect(response.status).to.be.equal(400);

					});
		});
	});
	describe("DELETE method", function () {
		it("should delete a plot point", function () {
			return client.delete(url)
					.then((response) => {
						expect(response.status).to.be.equal(200);
					})
					.then(() => PlotPoint.findById(original._id))
					.then((plotPoint) => expect(plotPoint).to.not.exist);
		});
	});
});