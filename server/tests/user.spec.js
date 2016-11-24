import {cleanDatabase, client, createAccount, expect, user} from "./support/fixtures";


describe('The user API', function () {

	let curUser = {};

	beforeEach(() => cleanDatabase());

	describe("/api/user", function () {

		describe("POST method", function () {
			it("must register a valid user", function () {
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

			it("must return a 400 for a duplicate user", function () {
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

			it("must return a 400 for a missing username", function () {
				return client.post("/api/user", {password: user.password})
						.then((response) => {
							expect(response.status).to.be.equal(400);
							return response.data;
						})
						.then((data) => {
							expect(data.error).to.be.equal("Username must be an email");
						});
			});

			it("must return a 400 for a missing password", function () {
				return client.post("/api/user", {username: user.username})
						.then((response) => {
							expect(response.status).to.be.equal(400);
							return response.data;
						})
						.then((data) => {
							expect(data.error).to.be.equal("No password was given");
						});
			});

			it("must return a 400 for an invalid username", function () {
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

	describe("/authenticate", function () {

		beforeEach(() => createAccount());

		describe("POST method", function () {
			it("must authenticates a valid user", function () {
				return client.post("/api/user/authenticate", user)
						.then((response) => {
							expect(response.status).to.be.equal(200);
							return response.data;
						})
						.then((data) => {
							expect(data.token).to.exist;
						});
			});

			it("must reject if the username is missing", function () {
				return client.post("/api/user/authenticate", {password: user.password})
						.then((response) => {
							expect(response.status).to.be.equal(400);
						});
			});

			it("must reject if the password is missing", function () {
				return client.post("/api/user/authenticate", {username: user.username})
						.then((response) => {
							expect(response.status).to.be.equal(400);
						});
			});

			it("must return a 401 when the username is wrong ", function () {
				return client.post("/api/user/authenticate", {username: "whingding", password: user.password})
						.then((response) => {
							expect(response.status).to.be.equal(401);
						});
			});

			it("must return a 401 when the password is wrong ", function () {
				return client.post("/api/user/authenticate", {username: user.username, password: "this is the wrong password"})
						.then((response) => {
							expect(response.status).to.be.equal(401);
						});
			});
		});
	});
});