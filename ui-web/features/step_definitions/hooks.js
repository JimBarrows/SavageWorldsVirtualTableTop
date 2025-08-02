import '@babel/polyfill'
import {After, AfterAll, Before, Status} from 'cucumber'
import {writeFile}             from 'fs'
import {Builder}               from 'selenium-webdriver'
import testUserHelper          from '../support/test-users'

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
		const timestamp = new Date().getTime()
		const filename = `screenshots/failed-${scenario.pickle.name.replace(/\s+/g, '-')}-${timestamp}.png`
		writeFile(filename, image, 'base64', function (err) {
			if (err) console.log('Screenshot error:', err)
			else console.log('Screenshot saved:', filename)
		})
	}
	await browser.close()
	await browser.quit()
})

// Clean up all test users after all tests are done
AfterAll(async function () {
	await testUserHelper.cleanupAllTestUsers()
})
