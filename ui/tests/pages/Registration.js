/**
 * Created by JimBarrows on 11/26/16.
 */
import Page from "./page";

class Registration extends Page {

	constructor(browser) {
		super(browser, "user/register");
	}

	get username() {
		return browser.element("#username");
	}

	get password() {
		return browser.element("#password");
	}

	get confirmPassword() {
		return browser.element("#confirmPassword");
	}

	get registerButton() {
		return browser.element("#registerButton");
	}

	get dangerAlert() {
		return browser.element(".alert-danger");
	}

	turnOffHtml5Validation() {
		browser.executeAsync(function (done) {
			document.getElementById("registrationForm").setAttribute("noValidate", "true");
			done();
		})
	}
}

export default Registration;