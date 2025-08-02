import '@babel/polyfill'
import {Given, Then, When} from 'cucumber'
import {By, until}         from 'selenium-webdriver'
import sleep               from 'sleep'

Given('I have started the application', async function () {
	const browser = this.browser
	await browser.get('http://localhost:3000')
	browser.wait(until.elementLocated(By.id('root')))
})

Given('I\'m presented with an authentication challenge', async function () {
	let usernames = await this.browser.findElements(By.name('username'))
	let passwords = await this.browser.findElements(By.name('password'))
	if (usernames.length == 0 || passwords.length == 0) {
		fail("No authentication challenge")
	}
})

// Username step removed - using email only for authentication

Given('I provide an email of {string}', async function (email) {
	this.credentials.email = email
	await this.browser.findElement(By.id('email')).sendKeys(email)
})

Given('I provide a password of {string}', async function (password) {
	this.credentials.password = password
	await this.browser.findElement(By.name('password')).sendKeys(password)
})

When('I authenticate', async function () {
	const button = await this.browser.findElement(By.xpath('//*[@id="root"]/div/div/div/form/div[2]/span[1]/button'))
	await button.click()
	sleep.sleep(2)
	this.browser.wait(until.elementLocated(By.id('PageHeader-PlotPointListPage')))
})

Then('I am directed to the list of plot points', async function () {
	this.browser.wait(until.elementLocated(By.id('PageHeader-PlotPointListPage')), 5000)
	let elements = await this.browser.findElement(By.id('PageHeader-PlotPointListPage'))
	if (!elements) {
		assert.fail("Not on the plot point list page")
	}
})
