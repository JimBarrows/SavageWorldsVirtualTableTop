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
	// Replace TIMESTAMP with actual timestamp for unique emails
	if (email && email.includes('TIMESTAMP')) {
		email = email.replace('TIMESTAMP', Date.now().toString())
	}
	
	this.credentials = this.credentials || {}
	this.credentials.email = email
	
	// Wait for the page to be ready
	await this.browser.wait(until.elementLocated(By.css('input[type="email"]')), 5000)
	
	// Try multiple selectors to find the email input
	let emailInput
	try {
		// Try by type first (most reliable)
		emailInput = await this.browser.findElement(By.css('input[type="email"]'))
	} catch (e) {
		try {
			// Then try by the form control id that bootstrap-react-components uses
			emailInput = await this.browser.findElement(By.id('FormControl-email-EmailFormGroup-email'))
		} catch (e2) {
			// Finally try by id
			emailInput = await this.browser.findElement(By.id('email'))
		}
	}
	
	// Clear and send keys
	await emailInput.clear()
	await emailInput.sendKeys(email)
})

Given('I provide a password of {string}', async function (password) {
	this.credentials = this.credentials || {}
	this.credentials.password = password
	
	// Wait for password field to be present
	await this.browser.wait(until.elementLocated(By.css('input[type="password"]')), 5000)
	
	// Try multiple selectors to find the password input
	let passwordInput
	try {
		// Find all password fields
		const passwordFields = await this.browser.findElements(By.css('input[type="password"]'))
		if (passwordFields.length > 0) {
			// Use the first password field (should be the main password field)
			passwordInput = passwordFields[0]
		} else {
			throw new Error('No password fields found')
		}
	} catch (e) {
		try {
			// Fallback to ID
			passwordInput = await this.browser.findElement(By.id('password'))
		} catch (e2) {
			// Final fallback
			passwordInput = await this.browser.findElement(By.id('FormControl-password-PasswordFormGroup-password'))
		}
	}
	
	// Clear and send keys
	await passwordInput.clear()
	await passwordInput.sendKeys(password)
})

Given('I confirm the password {string}', async function (password) {
	// Wait for confirm password field to be present
	await this.browser.wait(until.elementLocated(By.css('input[type="password"]')), 5000)
	
	// Find all password fields
	const passwordFields = await this.browser.findElements(By.css('input[type="password"]'))
	
	// The confirm password field is usually the second password field
	if (passwordFields.length >= 2) {
		const confirmPasswordField = passwordFields[1]
		await confirmPasswordField.clear()
		await confirmPasswordField.sendKeys(password)
	} else {
		throw new Error('Confirm password field not found')
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
