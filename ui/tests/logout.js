import {expect} from './support/fixtures';
import Login from './pages/Login';
import Application from './pages/Application';


describe('Logging out with username/password login', function () {

	let loginPage       = {};
	let applicationPage = {};

	beforeEach(() => {
		loginPage       = new Login(browser);
		applicationPage = new Application(browser);
		browser.createAccount();
		browser.loginToApplication();
	});

	it('should allow user to logout from the application page', function () {
		applicationPage.open();
		applicationPage.logoutLink().click();
		browser.waitUntil(loginPage.isCurrent, 9000, 'Didn't change to login page', 1000);
		expect(loginPage.isCurrent()).to.be.true;
	});
});