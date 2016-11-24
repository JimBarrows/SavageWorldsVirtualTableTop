import {client, expect, Promise, plotPoint, startTestHouseKeeping} from "./support/fixtures";
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
					[plotPoint("first", currentUser._id),
						plotPoint("second", currentUser._id),
						plotPoint("first unowned"),
						plotPoint("second  unowned")]
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
	});
});