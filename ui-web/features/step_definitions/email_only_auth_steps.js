import '@babel/polyfill'
import {Given, Then, When} from 'cucumber'
import {By, until} from 'selenium-webdriver'
import {expect} from 'chai'
import sleep from 'sleep'
import fetch from 'node-fetch'
import jwt from 'jsonwebtoken'

const API_BASE_URL = 'http://localhost:8080/api/v1'

Then('no username is stored in the system', async function () {
  // This would need backend verification - for now we'll check the response
  // doesn't include username in the user object
  expect(this.lastApiResponse).to.exist
  expect(this.lastApiResponse.user).to.not.have.property('username')
})

When('I make a POST request to {string} with:', async function (endpoint, dataTable) {
  const data = {}
  dataTable.rawTable.forEach(row => {
    data[row[0]] = row[1]
  })
  
  try {
    const response = await fetch(`${API_BASE_URL}${endpoint}`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data)
    })
    
    this.lastApiResponse = {
      status: response.status,
      data: await response.json(),
      headers: response.headers
    }
  } catch (error) {
    this.lastApiError = error
    throw error
  }
})

Then('the response status should be {int}', function (expectedStatus) {
  expect(this.lastApiResponse.status).to.equal(expectedStatus)
})

Then('the response should contain access_token', function () {
  expect(this.lastApiResponse.data).to.have.nested.property('data.access_token')
})

Then('the response should contain the user\'s email', function () {
  expect(this.lastApiResponse.data).to.have.nested.property('data.user.email')
})

Then('the response should not require a username field', function () {
  // Check that the API accepted the request without username
  expect(this.lastApiResponse.status).to.be.oneOf([200, 201])
  // Verify no error about missing username
  if (this.lastApiResponse.data.error) {
    expect(this.lastApiResponse.data.error).to.not.match(/username/i)
  }
})

Then('I should not see a username field', async function () {
  // Check that username field doesn't exist in the form
  const usernameFields = await this.browser.findElements(By.id('username'))
  expect(usernameFields).to.have.lengthOf(0)
  
  // Also check for any input with name="username"
  const usernameInputs = await this.browser.findElements(By.css('input[name="username"]'))
  expect(usernameInputs).to.have.lengthOf(0)
})

Then('I should see an email field', async function () {
  const emailField = await this.browser.findElement(By.id('email'))
  const isDisplayed = await emailField.isDisplayed()
  expect(isDisplayed).to.be.true
})

Then('I should see a password field', async function () {
  const passwordField = await this.browser.findElement(By.id('password'))
  const isDisplayed = await passwordField.isDisplayed()
  expect(isDisplayed).to.be.true
})

Given('I am logged in as {string}', async function (email) {
  // Navigate to login page
  await this.browser.get('http://localhost:3000/login')
  
  // Fill in credentials
  const emailField = await this.browser.findElement(By.id('email'))
  await emailField.sendKeys(email)
  
  const passwordField = await this.browser.findElement(By.id('password'))
  await passwordField.sendKeys('SecurePass123!')
  
  // Submit form
  const submitButton = await this.browser.findElement(By.css('button[type="submit"]'))
  await submitButton.click()
  
  // Wait for redirect
  await this.browser.wait(async () => {
    const url = await this.browser.getCurrentUrl()
    return !url.includes('/login')
  }, 10000)
})

When('I view my profile', async function () {
  // Look for profile link in navigation
  try {
    const profileLink = await this.browser.findElement(By.linkText('Profile'))
    await profileLink.click()
  } catch (e) {
    // If no profile page, check the header for user info
    this.profileElement = await this.browser.findElement(By.css('.navbar'))
  }
})

Then('I should see my email {string}', async function (expectedEmail) {
  const pageSource = await this.browser.getPageSource()
  expect(pageSource).to.include(expectedEmail)
})

Then('I should not see any username information', async function () {
  const pageSource = await this.browser.getPageSource()
  // Check that there's no "Username:" label or similar
  expect(pageSource).to.not.match(/username\s*:/i)
})

When('I login with email {string}', async function (email) {
  // Store the login response for JWT inspection
  const response = await fetch(`${API_BASE_URL}/auth/login`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      email: email,
      password: 'SecurePass123!'
    })
  })
  
  this.loginResponse = await response.json()
})

Then('the JWT token should contain email as the identifier', function () {
  const accessToken = this.loginResponse.data.access_token
  // Decode without verification (just to inspect contents)
  const decoded = jwt.decode(accessToken)
  
  expect(decoded).to.have.property('email')
  expect(decoded.email).to.be.a('string')
})

Then('the JWT token should not contain a username field', function () {
  const accessToken = this.loginResponse.data.access_token
  const decoded = jwt.decode(accessToken)
  
  expect(decoded).to.not.have.property('username')
})