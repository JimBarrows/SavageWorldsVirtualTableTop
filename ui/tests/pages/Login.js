/**
 * Created by JimBarrows on 11/26/16.
 */
import Page from './page';

class Login extends Page {

	constructor(browser) {
		super(browser, 'user/login');
	}

	username() {
		return browser.element('#username');
	}

	password() {
		return browser.element('#password');
	}

	login() {
		return browser.element('#loginButton');
	}

	dangerAlert() {
		return browser.element('.alert-danger');
	}

	get register() {
		return browser.element('#register');
	}
}

export default Login;