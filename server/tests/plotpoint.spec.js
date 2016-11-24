import {client, expect, Promise, plotPoint as createPlotPoint, startTestHouseKeeping} from "./support/fixtures";
import PlotPoint from "../models/PlotPoint";

describe("The plot points API", function () {

	let currentUser = {};

	beforeEach(() => startTestHouseKeeping()
			.then((account) => currentUser = account));

	describe("/api/plotpoints", function () {

		let url        = "/api/plotpoints";
		let plotPoints = [];

		describe("GET method", function () {

			let unownedPlotPoints = [];

			beforeEach(() => Promise.all(
					[createPlotPoint("first", currentUser._id),
						createPlotPoint("second", currentUser._id),
						createPlotPoint("first unowned"),
						createPlotPoint("second  unowned")]
							.map((pp) => PlotPoint.create(pp)))
					.then((responses) => responses.forEach((pp) => pp.user ? plotPoints.push(pp) : unownedPlotPoints.push(pp)))
			);
			it("returns a list of plot points for the user", function () {
				return client.get(url)
						.then((response) => {
							expect(response.status).to.be.equal(200);
							return response.data;
						})
						.then((data) => {
							expect(data).to.exist;
							expect(data).to.be.an('array');
							expect(data).to.have.lengthOf(plotPoints.length);
							expect(data.map((pp) => pp.name)).to.include.members(plotPoints.map((pp) => pp.name));
							expect(data.map((pp) => pp.name)).to.not.include.members(unownedPlotPoints.map((pp) => pp.name));
						});
			});
		});
		describe("POST method", function () {

			it("creates a valid plot point", function () {
				let plotPoint = createPlotPoint("perfectly good plot point", currentUser._id);
				return client.post(url, plotPoint)
						.then((response) => {
							expect(response.status).to.be.equal(201);
							return response.data;
						})
						.then((data) => {
							expect(data).to.exist;
							expect(data._id).to.exist;
							expect(data.name).to.be.equal(plotPoint.name);
						})
			});
			it("rejects a plot point without a name.", function () {
				let plotPoint = createPlotPoint("perfectly good plot point", currentUser._id);
				delete plotPoint.name;
				return client.post(url, plotPoint)
						.then((response) => {
							expect(response.status).to.be.equal(400);
							return response.data;
						})
						.then((data) => {
							expect(data).to.deep.equal({"error": {"validation": {"name": "is required"}}});
						});
			});
		});
	});
});