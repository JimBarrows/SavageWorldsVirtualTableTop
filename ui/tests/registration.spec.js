/**
 * Created by JimBarrows on 11/26/16.
 */
import {expect, user} from "./support/fixtures";
import Registration from "./pages/Registration";
import Application from "./pages/Application";

describe("How to register with username and password", function () {

	let registrationPage = {};
	let applicationPage  = {};

	beforeEach(() => {
		registrationPage = new Registration(browser);
		applicationPage  = new Application(browser);
		registrationPage.open();
	});

	it("should allow registration with valid username and password", function () {
		registrationPage.username.setValue(user.username);
		registrationPage.password.setValue(user.password);
		registrationPage.confirmPassword.setValue(user.password);
		registrationPage.registerButton.click();
		browser.waitUntil(applicationPage.isCurrent, 9000, "Didn't change to application page", 1000);
		expect(applicationPage.isCurrent()).to.be.true;
	});

	it("should reject a registration if the username is already in use", function () {
		browser.createAccount();
		registrationPage.username.setValue(user.username);
		registrationPage.password.setValue(user.password);
		registrationPage.confirmPassword.setValue(user.password);
		registrationPage.registerButton.click();
		registrationPage.dangerAlert.waitForExist(10000);
		expect(registrationPage.dangerAlert.getText()).to.be.equal("A user with the given username is already registered");
	});

	it("should reject a registration if the confirm password field does not match the password field", function () {
		registrationPage.username.setValue(user.username);
		registrationPage.password.setValue(user.password);
		registrationPage.confirmPassword.setValue("different password");
		registrationPage.registerButton.click();
		registrationPage.dangerAlert.waitForExist(10000);
		expect(registrationPage.dangerAlert.getText()).to.be.equal("Passwords do not match");
	});

	it("should reject a registration if the password field does not match the confirm password field", function () {
		registrationPage.username.setValue(user.username);
		registrationPage.password.setValue("different password");
		registrationPage.confirmPassword.setValue(user.password);
		registrationPage.registerButton.click();
		registrationPage.dangerAlert.waitForExist(10000);
		expect(registrationPage.dangerAlert.getText()).to.be.equal("Passwords do not match");
	});

	it("should require a username", function () {
		registrationPage.turnOffHtml5Validation();
		registrationPage.password.setValue(user.password);
		registrationPage.confirmPassword.setValue(user.password);
		registrationPage.registerButton.click();
		registrationPage.dangerAlert.waitForExist(10000);
		expect(registrationPage.dangerAlert.getText()).to.be.equal("No username was given");
	});

	it("should require password fields", function () {
		registrationPage.turnOffHtml5Validation();
		registrationPage.username.setValue(user.username);
		registrationPage.registerButton.click();
		registrationPage.dangerAlert.waitForExist(10000);
		expect(registrationPage.dangerAlert.getText()).to.be.equal("No password was given");
	});
});