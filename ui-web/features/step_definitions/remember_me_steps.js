import '@babel/polyfill'
import {Given, Then, When} from 'cucumber'
import {By, until} from 'selenium-webdriver'
import {expect} from 'chai'
import sleep from 'sleep'

Given('I have valid user credentials', async function () {
  // Use existing test user credentials
  this.credentials = {
    email: 'testuser@example.com',
    password: 'testpass123'
  }
})

Given('I enter my valid credentials', async function () {
  const browser = this.browser
  
  // Enter email
  const emailField = await browser.findElement(By.id('email'))
  await emailField.clear()
  await emailField.sendKeys(this.credentials.email)
  
  // Enter password
  const passwordField = await browser.findElement(By.id('password'))
  await passwordField.clear()
  await passwordField.sendKeys(this.credentials.password)
})

When('I check the {string} checkbox', async function (checkboxLabel) {
  const browser = this.browser
  
  // Find the Remember Me checkbox by its label or id
  let checkbox
  try {
    // Try to find by label text
    checkbox = await browser.findElement(By.xpath(`//label[contains(text(), '${checkboxLabel}')]/input[@type='checkbox']`))
  } catch (e) {
    try {
      // Try to find by adjacent label
      checkbox = await browser.findElement(By.xpath(`//input[@type='checkbox' and following-sibling::label[contains(text(), '${checkboxLabel}')]]`))
    } catch (e2) {
      // Try to find by id
      checkbox = await browser.findElement(By.id('rememberMe'))
    }
  }
  
  // Check if it's already checked
  const isChecked = await checkbox.isSelected()
  if (!isChecked) {
    await checkbox.click()
  }
  
  // Store that we checked remember me for later verification
  this.rememberMeChecked = true
})

When('I do not check the {string} checkbox', async function (checkboxLabel) {
  const browser = this.browser
  
  // Find the Remember Me checkbox
  let checkbox
  try {
    checkbox = await browser.findElement(By.xpath(`//label[contains(text(), '${checkboxLabel}')]/input[@type='checkbox']`))
  } catch (e) {
    try {
      checkbox = await browser.findElement(By.xpath(`//input[@type='checkbox' and following-sibling::label[contains(text(), '${checkboxLabel}')]]`))
    } catch (e2) {
      checkbox = await browser.findElement(By.id('rememberMe'))
    }
  }
  
  // Ensure it's unchecked
  const isChecked = await checkbox.isSelected()
  if (isChecked) {
    await checkbox.click()
  }
  
  this.rememberMeChecked = false
})

When('I click the login button', async function () {
  const browser = this.browser
  const loginButton = await browser.findElement(By.css('button[type="submit"]'))
  await loginButton.click()
  
  // Wait for login to complete
  sleep.sleep(2)
})

Then('I should be successfully logged in', async function () {
  const browser = this.browser
  
  // Wait for navigation away from login page
  await browser.wait(async () => {
    const url = await browser.getCurrentUrl()
    return !url.includes('/login')
  }, 10000, 'Failed to navigate away from login page')
  
  // Verify we're authenticated
  const currentUrl = await browser.getCurrentUrl()
  expect(currentUrl).to.not.include('/login')
})

Then('my session should be extended for remember me duration', async function () {
  const browser = this.browser
  
  // Check localStorage for extended token expiry
  const tokenData = await browser.executeScript(`
    const accessToken = localStorage.getItem('accessToken');
    const refreshToken = localStorage.getItem('refreshToken');
    const rememberMe = localStorage.getItem('rememberMe');
    return { accessToken, refreshToken, rememberMe };
  `)
  
  // Verify remember me flag is set
  expect(tokenData.rememberMe).to.equal('true')
  
  // Verify we have tokens
  expect(tokenData.accessToken).to.exist
  expect(tokenData.refreshToken).to.exist
  
  this.sessionType = 'extended'
})

Then('my session should use standard duration', async function () {
  const browser = this.browser
  
  // Check localStorage for standard session
  const tokenData = await browser.executeScript(`
    const accessToken = localStorage.getItem('accessToken');
    const refreshToken = localStorage.getItem('refreshToken');
    const rememberMe = localStorage.getItem('rememberMe');
    return { accessToken, refreshToken, rememberMe };
  `)
  
  // Verify remember me flag is not set or false
  expect(tokenData.rememberMe).to.not.equal('true')
  
  // Verify we have tokens
  expect(tokenData.accessToken).to.exist
  expect(tokenData.refreshToken).to.exist
  
  this.sessionType = 'standard'
})

Then('I should see the Plot Point list page', async function () {
  const browser = this.browser
  
  // Wait for the plot point list page to load
  await browser.wait(until.elementLocated(By.xpath("//h1[contains(text(), 'Plot Points')]")), 5000)
  
  // Verify we're on the correct page
  const header = await browser.findElement(By.xpath("//h1[contains(text(), 'Plot Points')]"))
  const isDisplayed = await header.isDisplayed()
  expect(isDisplayed).to.be.true
})

When('I visit the login page', async function () {
  const browser = this.browser
  await browser.get('http://localhost:3000/login')
  await browser.wait(until.elementLocated(By.css('form')), 5000)
})

Then('I should see a {string} checkbox', async function (checkboxLabel) {
  const browser = this.browser
  
  // Find the checkbox
  let checkbox
  try {
    checkbox = await browser.findElement(By.xpath(`//label[contains(text(), '${checkboxLabel}')]/input[@type='checkbox']`))
  } catch (e) {
    try {
      checkbox = await browser.findElement(By.xpath(`//input[@type='checkbox' and following-sibling::label[contains(text(), '${checkboxLabel}')]]`))
    } catch (e2) {
      checkbox = await browser.findElement(By.id('rememberMe'))
    }
  }
  
  const isDisplayed = await checkbox.isDisplayed()
  expect(isDisplayed).to.be.true
})

Then('the checkbox should be unchecked by default', async function () {
  const browser = this.browser
  
  const checkbox = await browser.findElement(By.id('rememberMe'))
  const isChecked = await checkbox.isSelected()
  expect(isChecked).to.be.false
})

Then('I should be able to check and uncheck the checkbox', async function () {
  const browser = this.browser
  
  const checkbox = await browser.findElement(By.id('rememberMe'))
  
  // Initially should be unchecked
  let isChecked = await checkbox.isSelected()
  expect(isChecked).to.be.false
  
  // Check the checkbox
  await checkbox.click()
  isChecked = await checkbox.isSelected()
  expect(isChecked).to.be.true
  
  // Uncheck the checkbox
  await checkbox.click()
  isChecked = await checkbox.isSelected()
  expect(isChecked).to.be.false
})

Given('I have logged in with {string} checked', async function (checkboxLabel) {
  const browser = this.browser
  
  // Navigate to login page
  await browser.get('http://localhost:3000/login')
  await browser.wait(until.elementLocated(By.css('form')), 5000)
  
  // Enter credentials
  const emailField = await browser.findElement(By.id('email'))
  await emailField.sendKeys(this.credentials.email)
  
  const passwordField = await browser.findElement(By.id('password'))
  await passwordField.sendKeys(this.credentials.password)
  
  // Check remember me
  const checkbox = await browser.findElement(By.id('rememberMe'))
  await checkbox.click()
  
  // Submit form
  const loginButton = await browser.findElement(By.css('button[type="submit"]'))
  await loginButton.click()
  
  // Wait for login to complete
  await browser.wait(async () => {
    const url = await browser.getCurrentUrl()
    return !url.includes('/login')
  }, 10000)
})

Given('I have logged in without {string} checked', async function (checkboxLabel) {
  const browser = this.browser
  
  // Navigate to login page
  await browser.get('http://localhost:3000/login')
  await browser.wait(until.elementLocated(By.css('form')), 5000)
  
  // Enter credentials
  const emailField = await browser.findElement(By.id('email'))
  await emailField.sendKeys(this.credentials.email)
  
  const passwordField = await browser.findElement(By.id('password'))
  await passwordField.sendKeys(this.credentials.password)
  
  // Ensure remember me is NOT checked
  const checkbox = await browser.findElement(By.id('rememberMe'))
  const isChecked = await checkbox.isSelected()
  if (isChecked) {
    await checkbox.click()
  }
  
  // Submit form
  const loginButton = await browser.findElement(By.css('button[type="submit"]'))
  await loginButton.click()
  
  // Wait for login to complete
  await browser.wait(async () => {
    const url = await browser.getCurrentUrl()
    return !url.includes('/login')
  }, 10000)
})

When('I close the browser', async function () {
  // Store current session info before closing
  const browser = this.browser
  this.sessionData = await browser.executeScript(`
    return {
      accessToken: localStorage.getItem('accessToken'),
      refreshToken: localStorage.getItem('refreshToken'),
      rememberMe: localStorage.getItem('rememberMe')
    };
  `)
  
  await browser.quit()
})

When('I reopen the browser and visit the application', async function () {
  // Reinitialize browser
  const {Builder} = require('selenium-webdriver')
  this.browser = new Builder().forBrowser('chrome').build()
  
  // Navigate to application
  await this.browser.get('http://localhost:3000')
  sleep.sleep(2)
})

Then('I should still be logged in', async function () {
  const browser = this.browser
  
  // Should be on authenticated page, not redirected to login
  const currentUrl = await browser.getCurrentUrl()
  expect(currentUrl).to.not.include('/login')
  
  // Should see plot points page
  await browser.wait(until.elementLocated(By.xpath("//h1[contains(text(), 'Plot Points')]")), 5000)
})

When('the standard session duration expires', async function () {
  // Simulate session expiration by removing tokens
  const browser = this.browser
  await browser.executeScript(`
    localStorage.removeItem('accessToken');
    localStorage.removeItem('refreshToken');
  `)
})

When('the extended session duration expires', async function () {
  // Simulate extended session expiration
  const browser = this.browser
  await browser.executeScript(`
    localStorage.removeItem('accessToken');
    localStorage.removeItem('refreshToken');
    localStorage.removeItem('rememberMe');
  `)
})

When('I try to access a protected page', async function () {
  const browser = this.browser
  await browser.get('http://localhost:3000/')
  sleep.sleep(2)
})

Then('I should be redirected to the login page', async function () {
  const browser = this.browser
  
  // Wait for redirect to login page
  await browser.wait(until.urlContains('/login'), 5000)
  const currentUrl = await browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
})

Then('I should see the login form', async function () {
  const browser = this.browser
  
  // Verify login form is visible
  await browser.wait(until.elementLocated(By.css('form')), 5000)
  const form = await browser.findElement(By.css('form'))
  const isDisplayed = await form.isDisplayed()
  expect(isDisplayed).to.be.true
})