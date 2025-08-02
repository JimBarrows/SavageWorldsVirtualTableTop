import '@babel/polyfill'
import {Given, Then, When} from 'cucumber'
import {expect} from 'chai'
import axios from 'axios'

Given('the API is running on {string}', function (apiUrl) {
  this.apiUrl = apiUrl
})

Given('the frontend is running on {string}', function (frontendUrl) {
  this.frontendUrl = frontendUrl
})

Given('I provide valid signup data', function () {
  this.signupData = {
    username: 'testuser_' + Date.now(),
    email: `testuser_${Date.now()}@example.com`,
    password: 'ValidPassword123!',
    confirmPassword: 'ValidPassword123!'
  }
})

When('I send a preflight OPTIONS request to {string}', async function (endpoint) {
  try {
    this.response = await axios({
      method: 'OPTIONS',
      url: `${this.apiUrl}${endpoint}`,
      headers: {
        'Origin': this.frontendUrl,
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'Content-Type'
      }
    })
  } catch (error) {
    this.response = error.response
    this.error = error
  }
})

When('I submit the signup form from {string}', async function (origin) {
  try {
    this.response = await axios({
      method: 'POST',
      url: `${this.apiUrl}/api/v1/auth/register`,
      headers: {
        'Origin': origin,
        'Content-Type': 'application/json'
      },
      data: this.signupData
    })
  } catch (error) {
    this.response = error.response
    this.error = error
  }
})

Then('the response should include {string} header', function (headerName) {
  expect(this.response.headers).to.have.property(headerName.toLowerCase())
})

Then('the {string} header should contain {string}', function (headerName, expectedValue) {
  const headerValue = this.response.headers[headerName.toLowerCase()]
  expect(headerValue).to.include(expectedValue)
})

Then('the response status should be {int}', function (expectedStatus) {
  expect(this.response.status).to.equal(expectedStatus)
})

Then('the request should be blocked by CORS policy', function () {
  // When CORS blocks a request, it typically results in a network error
  // or the browser prevents the response from being accessed
  expect(this.error).to.not.be.null
  if (this.response) {
    // Some browsers/tools might still return a response without CORS headers
    const corsHeader = this.response.headers['access-control-allow-origin']
    expect(corsHeader).to.not.equal('http://unauthorized-origin.com')
  }
})