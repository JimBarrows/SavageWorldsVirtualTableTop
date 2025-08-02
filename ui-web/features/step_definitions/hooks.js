import '@babel/polyfill'
import {After, Before, Status} from 'cucumber'
import {writeFile}             from 'fs'
import {Builder}               from 'selenium-webdriver'

Before(async function () {
	this.browser = await new Builder()
		.forBrowser('chrome')
		.build()
	await this.browser.manage().setTimeouts({implicit: 45 * 1000, pageLoad: 45 * 1000})
})

After(async function (scenario) {
	const browser = this.browser
	
	// Clean up any test data created during the scenario
	// Since we're not using Amplify, this would be handled by your backend API
	// TODO: Add cleanup logic based on your actual backend implementation
	
	if (scenario.result.status === Status.FAILED) {
		const image = await browser.takeScreenshot()
		writeFile('out.png', image, 'base64', function (err) {
			console.log(err)
		})
	}
	await browser.close()
	await browser.quit()
})
