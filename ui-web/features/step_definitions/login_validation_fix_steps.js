import '@babel/polyfill'
import {Given, Then, When} from 'cucumber'
import {expect} from 'chai'
import fetch from 'node-fetch'
import {testUserHelper} from '../support/test-users'

const API_BASE_URL = 'http://localhost:8080/api/v1'

// Create test users for validation scenarios
Given('a user exists with email {string} and password {string}', async function (email, password) {
  try {
    await testUserHelper.createTestUser(email, password)
  } catch (error) {
    // User might already exist, that's ok
    console.log(`User ${email} might already exist:`, error.message)
  }
})

When('I make a POST request to {string} with JSON:', async function (endpoint, jsonString) {
  try {
    // Parse the JSON string
    const data = JSON.parse(jsonString)
    
    // Remove /api/v1 from endpoint if it's already there
    const cleanEndpoint = endpoint.replace(/^\/api\/v1/, '')
    
    const response = await fetch(`${API_BASE_URL}${cleanEndpoint}`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data)
    })
    
    // Store response for assertions
    const responseText = await response.text()
    let responseData
    try {
      responseData = JSON.parse(responseText)
    } catch (e) {
      responseData = { rawText: responseText }
    }
    
    this.lastApiResponse = {
      status: response.status,
      data: responseData,
      headers: response.headers
    }
  } catch (error) {
    this.lastApiError = error
    throw error
  }
})

Then('the response should contain error message about missing email', function () {
  expect(this.lastApiResponse.status).to.equal(400)
  
  // Check for validation error in response
  const errorMessage = this.lastApiResponse.data.error || 
                      this.lastApiResponse.data.message || 
                      this.lastApiResponse.data.details ||
                      ''
  
  // The error should mention 'email' and 'required'
  expect(errorMessage.toLowerCase()).to.include('email')
  expect(errorMessage.toLowerCase()).to.match(/required|missing|empty/)
})

Then('the error message should contain {string}', function (expectedMessage) {
  const errorMessage = this.lastApiResponse.data.error || 
                      this.lastApiResponse.data.message || 
                      this.lastApiResponse.data.details ||
                      JSON.stringify(this.lastApiResponse.data)
  
  expect(errorMessage).to.include(expectedMessage)
})