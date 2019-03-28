import '@babel/polyfill'
import {After, Before, Status} from 'cucumber'
import {writeFile}             from 'fs'
import {Builder}               from 'selenium-webdriver'

Before(async function () {
	this.browser = await new Builder()
		.forBrowser('chrome')
		.build()
})

After(async function (scenario) {
	const browser = this.browser
	if (scenario.result.status === Status.Failed) {
		const image = await browser.takeScreenshot()
		writeFile('out.png', image, 'base64', function (err) {
			console.log(err)
		})
	}
	await browser.close()
	await browser.quit()
})
