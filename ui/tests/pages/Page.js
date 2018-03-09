/**
 * Created by JimBarrows on 11/26/16.
 */
export default class Page {

	constructor(browser, path = "") {
		this.browser = browser;
		this.path    = "/" + path;
	}

	open() {
		this.browser.url(this.path);
		this.browser.waitUntil(this.isCurrent, 15000, "Didn't change to " + this.path, 1000);
	}

	isCurrent() {
		return browser.getUrl() === `http://localhost:8080/${this.path}`;
	}

	logoutLink() {
		return this.browser.element("#logout");
	}
};