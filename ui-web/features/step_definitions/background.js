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
	let emails = await this.browser.findElements(By.id('email'))
	let passwords = await this.browser.findElements(By.name('password'))
	if (emails.length == 0 || passwords.length == 0) {
		fail("No authentication challenge")
	}
})

// Username step removed - using email only for authentication

Given('I provide an email of {string}', async function (email) {
	this.credentials = this.credentials || {}
	this.credentials.email = email
	// Try multiple selectors to find the email input
	try {
		// First try by id
		await this.browser.findElement(By.id('email')).sendKeys(email)
	} catch (e) {
		try {
			// Then try by the form control id that bootstrap-react-components uses
			await this.browser.findElement(By.id('FormControl-email-EmailFormGroup-email')).sendKeys(email)
		} catch (e2) {
			// Finally try by type
			await this.browser.findElement(By.css('input[type="email"]')).sendKeys(email)
		}
	}
})

Given('I provide a password of {string}', async function (password) {
	this.credentials = this.credentials || {}
	this.credentials.password = password
	// Try multiple selectors to find the password input
	try {
		// First try by name
		await this.browser.findElement(By.name('password')).sendKeys(password)
	} catch (e) {
		try {
			// Then try by id
			await this.browser.findElement(By.id('password')).sendKeys(password)
		} catch (e2) {
			try {
				// Try by the form control id that bootstrap-react-components uses
				await this.browser.findElement(By.id('FormControl-password-PasswordFormGroup-password')).sendKeys(password)
			} catch (e3) {
				// Finally try by type and not confirm password
				const passwordFields = await this.browser.findElements(By.css('input[type="password"]'))
				if (passwordFields.length > 0) {
					await passwordFields[0].sendKeys(password)
				} else {
					throw new Error('Could not find password field')
				}
			}
		}
	}
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
