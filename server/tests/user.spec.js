import {cleanDatabase, client, createAccount, expect, user} from "./support/fixtures";


describe('The user API', function () {

	let curUser = {};

	beforeEach(() => cleanDatabase());

	describe("can register a user", function () {

		it("returning a json web token", function () {
			return client
					.post("/api/user", user)
					.then((response) => {
						expect(response.status).to.be.equal(200);
						return response.data;
					})
					.then((data) => {
						expect(data.token).to.exist;
					});
		});
	});

	describe("will handle invalid user registrations", function () {

		it("should return a 400 for a duplicate user", function () {
			return createAccount()
					.then(() => client.post("/api/user", user))
					.then((response) => {
						expect(response.status).to.be.equal(400);
						return response.data;
					})
					.then((data) => {
						expect(data.error).to.be.equal("A user with the given username is already registered");
					});
		});

		it("should return a 400 for a missing username", function () {
			return client.post("/api/user", {password: user.password})
					.then((response) => {
						expect(response.status).to.be.equal(400);
						return response.data;
					})
					.then((data) => {
						expect(data.error).to.be.equal("Username must be an email");
					});
		});

		it("should return a 400 for a missing password", function () {
			return client.post("/api/user", {username: user.username})
					.then((response) => {
						expect(response.status).to.be.equal(400);
						return response.data;
					})
					.then((data) => {
						expect(data.error).to.be.equal("No password was given");
					});
		});

		it("should return a 400 for an invalid username", function () {
			return client.post("/api/user", {username: "goobersnot", password: user.password})
					.then((response) => {
						expect(response.status).to.be.equal(400);
						return response.data;
					})
					.then((data) => {
						expect(data.error).to.be.equal("Username must be an email");
					});
		});
	});
});