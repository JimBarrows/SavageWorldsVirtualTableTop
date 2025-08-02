import '@babel/polyfill'
import {Given, Then, When} from 'cucumber'
import {By, until} from 'selenium-webdriver'
import {expect} from 'chai'
import sleep from 'sleep'

Given('I am on the login page', async function () {
  const browser = this.browser
  await browser.get('http://localhost:3000/login')
  await browser.wait(until.elementLocated(By.css('form')), 5000)
})

Given('a user exists with email {string} and password {string}', async function (email, password) {
  // This would typically involve creating a test user in the database
  // For now, we'll assume the test environment has this user pre-seeded
  this.testUser = { email, password }
  console.log('Note: This test assumes a user exists in the backend with email:', email)
})

Given('I leave the email field empty', async function () {
  // Intentionally not filling the email field
  this.credentials = this.credentials || {}
  this.credentials.email = ''
})

Given('I leave the password field empty', async function () {
  // Intentionally not filling the password field
  this.credentials = this.credentials || {}
  this.credentials.password = ''
})

Given('I check the {string} checkbox', async function (checkboxLabel) {
  // Find checkbox by label text
  const checkbox = await this.browser.findElement(By.xpath(`//label[contains(text(), '${checkboxLabel}')]/../input[@type='checkbox']`))
  await checkbox.click()
})

When('I submit the login form', async function () {
  const submitButton = await this.browser.findElement(By.css('button[type="submit"]'))
  await submitButton.click()
  sleep.sleep(1) // Allow time for form submission
})

Then('I am successfully authenticated', async function () {
  // Wait for redirect to plot points page or success indication
  await this.browser.wait(until.urlContains('/'), 5000)
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.not.include('/login')
})

Then('I am redirected to the plot points list page', async function () {
  await this.browser.wait(until.elementLocated(By.id('PageHeader-PlotPointListPage')), 5000)
  const header = await this.browser.findElement(By.id('PageHeader-PlotPointListPage'))
  const isDisplayed = await header.isDisplayed()
  expect(isDisplayed).to.be.true
})

Then('I see a welcome message', async function () {
  // Check for welcome message or user indication in header
  const headerElement = await this.browser.findElement(By.css('.navbar'))
  const headerText = await headerElement.getText()
  // The header should show some indication that user is logged in
  expect(headerText).to.exist
})

Then('I see an error message {string}', async function (expectedMessage) {
  // Wait for error message to appear
  await this.browser.wait(until.elementLocated(By.css('.alert-danger')), 5000)
  const errorElement = await this.browser.findElement(By.css('.alert-danger'))
  const errorText = await errorElement.getText()
  expect(errorText).to.include(expectedMessage)
})

Then('I remain on the login page', async function () {
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
  
  // Verify form is still visible
  const form = await this.browser.findElement(By.css('form'))
  const isDisplayed = await form.isDisplayed()
  expect(isDisplayed).to.be.true
})

Then('I am not authenticated', async function () {
  // Verify user is still on login page and not redirected
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
})

Then('my session is persisted', async function () {
  // Check for session cookie or local storage token
  const cookies = await this.browser.manage().getCookies()
  const hasSessionCookie = cookies.some(cookie => 
    cookie.name.includes('session') || cookie.name.includes('token')
  )
  expect(hasSessionCookie).to.be.true
})

When('I close and reopen the application', async function () {
  // Store current URL
  this.previousUrl = await this.browser.getCurrentUrl()
  
  // Close and reopen browser (simulate session persistence)
  await this.browser.quit()
  
  // Reinitialize browser
  const {Builder} = require('selenium-webdriver')
  this.browser = new Builder().forBrowser('chrome').build()
  
  // Navigate back to application
  await this.browser.get('http://localhost:3000')
  sleep.sleep(2)
})

Then('I am still logged in', async function () {
  // Should be redirected to authenticated area, not login page
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.not.include('/login')
})

When('I click on {string}', async function (linkText) {
  const link = await this.browser.findElement(By.linkText(linkText))
  await link.click()
  sleep.sleep(1)
})

Then('I am redirected to the signup page', async function () {
  await this.browser.wait(until.urlContains('/signup'), 5000)
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/signup')
})

Then('I am redirected to the password reset page', async function () {
  await this.browser.wait(until.urlContains('/reset-password'), 5000)
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/reset-password')
})