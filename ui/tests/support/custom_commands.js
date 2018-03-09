/**
 * Created by JimBarrows on 11/26/16.
 */
import PlotPointList from "../pages/PlotPointList";
import {createAccount, user} from "./fixtures";
import Login from "../pages/Login";

console.log("custom_commands");

browser.addCommand('createAccount', function async() {
	console.log("custom create account");
	return createAccount();
});

browser.addCommand("loginToApplication", function () {
	let plotPointList = new PlotPointList(browser);
	let loginPage     = new Login(browser);
	loginPage.open();
	browser.waitUntil(loginPage.isCurrent, 9000, "Didn't change to login page", 1000);
	loginPage.username().setValue(user.username);
	loginPage.password().setValue(user.password);
	loginPage.login().click();
	browser.waitUntil(plotPointList.isCurrent, 9000, "Didn't change to application page", 1000);
});
