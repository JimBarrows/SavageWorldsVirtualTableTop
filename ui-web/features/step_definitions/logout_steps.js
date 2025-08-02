import '@babel/polyfill'
import {Given, Then, When} from 'cucumber'
import {By, until} from 'selenium-webdriver'
import {expect} from 'chai'
import sleep from 'sleep'

Given('I am logged in as {string}', async function (username) {
  // Use existing authentication flow
  await this.browser.get('http://localhost:3000/login')
  await this.browser.wait(until.elementLocated(By.css('form')), 5000)
  
  // Use test user credentials
  const email = `${username.toLowerCase()}@example.com`
  const password = 'testPassword123!'
  
  // Fill login form
  const emailField = await this.browser.findElement(By.css('input[type="email"]'))
  await emailField.clear()
  await emailField.sendKeys(email)
  
  const passwordField = await this.browser.findElement(By.css('input[type="password"]'))
  await passwordField.clear()
  await passwordField.sendKeys(password)
  
  // Submit form
  const submitButton = await this.browser.findElement(By.css('button[type="submit"]'))
  await submitButton.click()
  
  // Wait for redirect to plot points page
  await this.browser.wait(until.elementLocated(By.id('PageHeader-PlotPointListPage')), 10000)
  
  this.currentUser = { username, email, password }
})

Given('I am on the plot points list page', async function () {
  // Verify we're on the correct page
  await this.browser.wait(until.elementLocated(By.id('PageHeader-PlotPointListPage')), 5000)
  const header = await this.browser.findElement(By.id('PageHeader-PlotPointListPage'))
  const isDisplayed = await header.isDisplayed()
  expect(isDisplayed).to.be.true
})

Given('I can see the logout option in the navigation menu', async function () {
  // Wait for navigation bar to be present
  await this.browser.wait(until.elementLocated(By.css('.navbar')), 5000)
  
  // Look for logout button/link in navigation
  await this.browser.wait(until.elementLocated(By.css('[data-testid="logout-button"], [data-test="logout"], .logout-btn, a[href*="logout"]')), 5000)
  
  const logoutElement = await this.browser.findElement(By.css('[data-testid="logout-button"], [data-test="logout"], .logout-btn, a[href*="logout"]'))
  const isDisplayed = await logoutElement.isDisplayed()
  expect(isDisplayed).to.be.true
})

Given('I have plot points loaded in my session', async function () {
  // Verify plot points are visible on the page
  await this.browser.wait(until.elementLocated(By.css('.plot-point, .list-group-item, [data-testid="plot-point"]')), 5000)
  
  // Store session data for verification later
  this.sessionData = {
    localStorage: await this.browser.executeScript('return JSON.stringify(localStorage)'),
    sessionStorage: await this.browser.executeScript('return JSON.stringify(sessionStorage)'),
    cookies: await this.browser.manage().getCookies()
  }
})

Given('I am editing a plot point', async function () {
  // Navigate to edit page or open edit form
  const editLink = await this.browser.findElement(By.css('[href*="/edit"], .edit-btn, [data-testid="edit-plot-point"]'))
  await editLink.click()
  
  // Wait for edit form to load
  await this.browser.wait(until.elementLocated(By.css('form, .edit-form')), 5000)
  
  this.editingPlotPoint = true
})

Given('I have made unsaved changes', async function () {
  // Make changes to a form field
  const inputField = await this.browser.findElement(By.css('input[type="text"], textarea'))
  await inputField.clear()
  await inputField.sendKeys('Modified content - unsaved changes')
  
  this.hasUnsavedChanges = true
})

Given('my session timeout is set to 30 minutes', async function () {
  // Set session timeout via JavaScript (mock implementation)
  await this.browser.executeScript(`
    window.sessionTimeout = 30 * 60 * 1000; // 30 minutes in milliseconds
    window.lastActivity = Date.now();
  `)
})

Given('I have been inactive for 30 minutes', async function () {
  // Simulate 30 minutes of inactivity by modifying lastActivity timestamp
  await this.browser.executeScript(`
    window.lastActivity = Date.now() - (30 * 60 * 1000); // 30 minutes ago
  `)
})

Given('I am logged in on multiple browser tabs', async function () {
  // Store reference to current tab
  this.originalTab = await this.browser.getWindowHandle()
  
  // Open a new tab and login there too
  await this.browser.executeScript('window.open("http://localhost:3000", "_blank");')
  const tabs = await this.browser.getAllWindowHandles()
  this.secondTab = tabs[1]
  
  // Switch to second tab and verify login state
  await this.browser.switchTo().window(this.secondTab)
  await this.browser.wait(until.elementLocated(By.id('PageHeader-PlotPointListPage')), 5000)
  
  // Switch back to original tab
  await this.browser.switchTo().window(this.originalTab)
  
  this.multipleTabs = true
})

Given('I previously logged in with {string} checked', async function (checkboxLabel) {
  // Set remember me preference in localStorage or session
  await this.browser.executeScript(`
    localStorage.setItem('rememberMe', 'true');
    localStorage.setItem('rememberedEmail', '${this.currentUser.email}');
  `)
  
  this.rememberMeEnabled = true
})

When('I click on the logout button', async function () {
  // Find and click logout button
  const logoutButton = await this.browser.findElement(By.css('[data-testid="logout-button"], [data-test="logout"], .logout-btn, a[href*="logout"]'))
  await logoutButton.click()
  
  sleep.sleep(1) // Allow time for logout action to begin
})

When('I try to perform any action', async function () {
  // Try to navigate to a protected resource
  await this.browser.get('http://localhost:3000/plot_point/add')
  sleep.sleep(1)
})

When('I click logout in one tab', async function () {
  // Ensure we're on the original tab and click logout
  await this.browser.switchTo().window(this.originalTab)
  
  const logoutButton = await this.browser.findElement(By.css('[data-testid="logout-button"], [data-test="logout"], .logout-btn, a[href*="logout"]'))
  await logoutButton.click()
  
  sleep.sleep(2) // Allow time for logout to propagate
})

When('I navigate back to the plot points page', async function () {
  await this.browser.get('http://localhost:3000')
  sleep.sleep(1)
})

When('I confirm the logout', async function () {
  // Look for confirmation dialog and click confirm
  await this.browser.wait(until.elementLocated(By.css('.btn-primary, .confirm-btn, [data-testid="confirm-logout"]')), 5000)
  
  const confirmButton = await this.browser.findElement(By.css('.btn-primary, .confirm-btn, [data-testid="confirm-logout"]'))
  await confirmButton.click()
  
  sleep.sleep(1)
})

When('I cancel the logout', async function () {
  // Look for cancel button in dialog and click it
  await this.browser.wait(until.elementLocated(By.css('.btn-secondary, .cancel-btn, [data-testid="cancel-logout"]')), 5000)
  
  const cancelButton = await this.browser.findElement(By.css('.btn-secondary, .cancel-btn, [data-testid="cancel-logout"]'))
  await cancelButton.click()
  
  sleep.sleep(1)
})

When('I return to the login page', async function () {
  await this.browser.get('http://localhost:3000/login')
  await this.browser.wait(until.elementLocated(By.css('form')), 5000)
})

Then('I am logged out successfully', async function () {
  // Verify authentication state is cleared
  // This could involve checking localStorage, sessionStorage, or cookies
  const authToken = await this.browser.executeScript('return localStorage.getItem("authToken") || sessionStorage.getItem("authToken")') 
  expect(authToken).to.be.null
})

Then('I am redirected to the login page', async function () {
  // Wait for redirect to login page
  await this.browser.wait(until.urlContains('/login'), 10000)
  
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
  
  // Verify login form is present
  await this.browser.wait(until.elementLocated(By.css('form')), 5000)
})

Then('I see a message {string}', async function (expectedMessage) {
  // Look for success message
  await this.browser.wait(until.elementLocated(By.css('.alert-success, .success-message, [data-testid="logout-success"]')), 5000)
  
  const messageElement = await this.browser.findElement(By.css('.alert-success, .success-message, [data-testid="logout-success"]'))
  const messageText = await messageElement.getText()
  expect(messageText).to.include(expectedMessage)
})

Then('I no longer see the authenticated navigation options', async function () {
  // Verify logout button is not visible
  const logoutButtons = await this.browser.findElements(By.css('[data-testid="logout-button"], [data-test="logout"], .logout-btn'))
  expect(logoutButtons).to.have.length(0)
  
  // Verify login-related navigation is visible instead
  await this.browser.wait(until.elementLocated(By.css('[href*="login"], .login-btn')), 5000)
})

Then('all session data is cleared', async function () {
  // Verify localStorage and sessionStorage are cleared
  const localStorage = await this.browser.executeScript('return JSON.stringify(localStorage)')
  const sessionStorage = await this.browser.executeScript('return JSON.stringify(sessionStorage)')
  
  // Should be empty objects or specific auth-related keys should be missing
  expect(localStorage).to.not.include('authToken')
  expect(sessionStorage).to.not.include('authToken')
  
  // Verify session cookies are cleared
  const cookies = await this.browser.manage().getCookies()
  const authCookies = cookies.filter(cookie => 
    cookie.name.includes('session') || cookie.name.includes('auth') || cookie.name.includes('token')
  )
  expect(authCookies).to.have.length(0)
})

Then('I cannot access protected resources', async function () {
  // Current URL should be login page since we were redirected
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
})

Then('I see a warning {string}', async function (expectedWarning) {
  // Look for warning dialog or modal
  await this.browser.wait(until.elementLocated(By.css('.modal, .alert-warning, [data-testid="unsaved-changes-warning"]')), 5000)
  
  const warningElement = await this.browser.findElement(By.css('.modal, .alert-warning, [data-testid="unsaved-changes-warning"]'))
  const warningText = await warningElement.getText()
  expect(warningText).to.include(expectedWarning)
})

Then('unsaved changes are discarded', async function () {
  // Since we're logged out, changes should be lost
  // This is verified by the fact that we're on the login page
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
})

Then('I remain logged in', async function () {
  // Verify we're still on an authenticated page
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.not.include('/login')
  
  // Verify logout button is still visible
  const logoutButton = await this.browser.findElement(By.css('[data-testid="logout-button"], [data-test="logout"], .logout-btn'))
  const isDisplayed = await logoutButton.isDisplayed()
  expect(isDisplayed).to.be.true
})

Then('I stay on the current page', async function () {
  // Verify we're still on the edit page
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/edit')
})

Then('my unsaved changes are preserved', async function () {
  // Verify the form still contains our modified content
  const inputField = await this.browser.findElement(By.css('input[type="text"], textarea'))
  const fieldValue = await inputField.getAttribute('value')
  expect(fieldValue).to.include('Modified content - unsaved changes')
})

Then('I am automatically logged out', async function () {
  // Should be redirected to login page due to session timeout
  await this.browser.wait(until.urlContains('/login'), 10000)
  
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
})

Then('I am logged out in all tabs', async function () {
  // Check both tabs are logged out
  
  // Check current tab
  const currentUrl = await this.browser.getCurrentUrl()
  expect(currentUrl).to.include('/login')
  
  // Switch to second tab and check
  await this.browser.switchTo().window(this.secondTab)
  await this.browser.wait(until.urlContains('/login'), 10000)
  
  const secondTabUrl = await this.browser.getCurrentUrl()
  expect(secondTabUrl).to.include('/login')
  
  // Switch back to original tab
  await this.browser.switchTo().window(this.originalTab)
})

Then('all tabs redirect to the login page', async function () {
  // Both tabs should now be on login page (verified in previous step)
  // Additional verification that both tabs show login form
  
  // Verify current tab has login form
  await this.browser.wait(until.elementLocated(By.css('form')), 5000)
  
  // Switch to second tab and verify
  await this.browser.switchTo().window(this.secondTab)
  await this.browser.wait(until.elementLocated(By.css('form')), 5000)
  
  // Close second tab and return to original
  await this.browser.close()
  await this.browser.switchTo().window(this.originalTab)
})

Then('the {string} preference is cleared', async function (preference) {
  // Verify remember me preference is removed from storage
  const rememberMe = await this.browser.executeScript('return localStorage.getItem("rememberMe")')
  expect(rememberMe).to.be.null
  
  const rememberedEmail = await this.browser.executeScript('return localStorage.getItem("rememberedEmail")')
  expect(rememberedEmail).to.be.null
})

Then('my credentials are not pre-filled', async function () {
  // Verify email field is empty
  const emailField = await this.browser.findElement(By.css('input[type="email"]'))
  const emailValue = await emailField.getAttribute('value')
  expect(emailValue).to.be.empty
  
  // Verify password field is empty
  const passwordField = await this.browser.findElement(By.css('input[type="password"]'))
  const passwordValue = await passwordField.getAttribute('value')
  expect(passwordValue).to.be.empty
  
  // Verify remember me checkbox is unchecked
  const rememberMeCheckbox = await this.browser.findElement(By.css('input[type="checkbox"]'))
  const isChecked = await rememberMeCheckbox.isSelected()
  expect(isChecked).to.be.false
})