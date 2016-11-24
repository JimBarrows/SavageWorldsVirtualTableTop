'use strict';
const mongoose = require('mongoose')
		, Account  = require("../models/account.js")
		, axios    = require("axios");

describe('User functionality', function () {

	var user = {
		username: 'ChesterTester'
		, password: 'testychesty'
	};

	beforeAll(function () {
		this.axios = axios.create({
			baseURL: 'http://localhost:3000/api',
			timeout: 1000
		});
	});

	beforeEach(function (done) {
		Account.remove({}, function (err, data) {
			if (err) {
				console.log("Error: " + err);
			}
			done();
		});

	});

	describe("user registration", function () {

		it("allows the service client to create a user", function (done) {
			let newUserId = '';
			this.axios.post('/user/register', user)
					.then((response) = > {
				const newUser = response.data;
			expect(newUser.username).toBe(user.username);
			expect(newUser.password).toBeUndefined();
			expect(newUser.id).toBeDefined();
			newUserId = newUser.id;
			return Account.findOne({username: user.username}).exec();
		})
			.
			then(function (dbUser) {
				expect(dbUser).toBeDefined();
				expect(dbUser.username).toBe(user.username);
				expect(dbUser.id).toBe(newUserId);
				done();
			})
					.catch((error) = > console.log(error);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			)
		});
	});

	describe("user login", function () {
		it("Should allow a user login", function (done) {
			let newUser = {};
			this.axios.post('/user/register', user)
					.then((response) = > {
				newUser   = response.data;
			return this.axios.post('/user/login', user);
		})
			.
			then((response) = > {
				let loggedInUser = response.data;
			expect(loggedInUser.username).toBe(user.username);
			expect(loggedInUser.id).toBe(newUser.id);
			expect(loggedInUser.password).toBeUndefined();
			done();
		})
		})

	})
});