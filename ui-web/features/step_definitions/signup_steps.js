import '@babel/polyfill'
import {Given, Then, When} from 'cucumber'
import {By, until} from 'selenium-webdriver'
import {expect} from 'chai'
import sleep from 'sleep'

Given('I am on the signup page', async function () {
  const browser = this.browser
  await browser.get('http://localhost:3000/signup')
  await browser.wait(until.elementLocated(By.id('signup-form')), 5000)
})

Given('I provide an email of {string}', async function (email) {
  this.signupData = this.signupData || {}
  this.signupData.email = email
  await this.browser.findElement(By.name('email')).sendKeys(email)
})

Given('I confirm the password with {string}', async function (confirmPassword) {
  this.signupData = this.signupData || {}
  this.signupData.confirmPassword = confirmPassword
  await this.browser.findElement(By.name('confirmPassword')).sendKeys(confirmPassword)
})

Given('a user {string} already exists', async function (username) {
  // This would typically involve creating a test user in the database
  // For now, we'll assume the test environment has this user pre-seeded
  this.existingUsername = username
})

Given('I leave the username field empty', async function () {
  // Intentionally not filling the username field
  this.signupData = this.signupData || {}
  this.signupData.username = ''
})

When('I submit the signup form', async function () {
  const submitButton = await this.browser.findElement(By.css('button[type="submit"]'))
  await submitButton.click()
  sleep.sleep(1) // Allow time for form submission
})

Then('the account is created successfully', async function () {
  // Wait for success indication
  await this.browser.wait(until.elementLocated(By.css('.alert-success')), 5000)
})

Then('I receive a confirmation message', async function () {
  const successMessage = await this.browser.findElement(By.css('.alert-success'))
  const text = await successMessage.getText()
  expect(text).to.include('successfully')
})

Then('I am redirected to the login page', async function () {
  await this.browser.wait(until.urlContains('/login'), 5000)
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
})

Then('I see an error message {string}', async function (expectedError) {
  await this.browser.wait(until.elementLocated(By.css('.alert-danger')), 5000)
  const errorElement = await this.browser.findElement(By.css('.alert-danger'))
  const errorText = await errorElement.getText()
  expect(errorText).to.include(expectedError)
})

Then('the account is not created', async function () {
  // Verify we're still on the signup page
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/signup')
  
  // Verify form is still visible
  const form = await this.browser.findElement(By.id('signup-form'))
  const isDisplayed = await form.isDisplayed()
  expect(isDisplayed).to.be.true
})

Then('I see an error message about password requirements', async function () {
  await this.browser.wait(until.elementLocated(By.css('.alert-danger')), 5000)
  const errorElement = await this.browser.findElement(By.css('.alert-danger'))
  const errorText = await errorElement.getText()
  // AWS Cognito typically requires passwords to be at least 8 characters with numbers and special characters
  expect(errorText.toLowerCase()).to.match(/password|requirement|policy|must/)
})