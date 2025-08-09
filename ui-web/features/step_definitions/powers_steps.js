import {Given, Then, When} from 'cucumber'
import {By, until} from 'selenium-webdriver'
import sleep from 'sleep'

Then('the plot point has default powers', async function () {
  // Verify that default powers exist in the plot point
  // This will be verified after backend implementation
  console.log('TODO: Verify plot point has default powers via backend API')
})

Given('I have a plot point named {string}', async function (plotPointName) {
  const browser = this.browser
  await browser.get('http://localhost:3000')
  browser.wait(until.elementLocated(By.id('root')))
  // Navigate to the plot point edit page
  // This assumes the plot point exists in the list
  const plotPointLink = await browser.findElement(By.linkText(plotPointName))
  await plotPointLink.click()
  this.currentPlotPoint = { name: plotPointName }
})

Given('I want to add a power to the plot point', async function () {
  // Navigate to powers tab
  const powersTab = await this.browser.findElement(By.id('NavItem-Navigation-PlotPoint-plotPointAdd-powers'))
  await powersTab.click()
  await this.browser.wait(until.elementLocated(By.id('Powers-PlotPoint-plotPointAdd')))
  
  // Click add power button
  const addPowerButton = await this.browser.findElement(By.id('button-Powers-PlotPoint-plotPointAdd'))
  await addPowerButton.click()
  
  // Initialize power object for this scenario
  this.currentPower = {
    modifiers: []
  }
})

Given('the power has a name of {string}', async function (powerName) {
  this.currentPower.name = powerName
  const nameElement = await this.browser.findElement(By.id('FormControl-text-TextFormGroup-PowerEditor-Powers-PlotPoint-plotPointAdd-0-Name'))
  await nameElement.clear()
  await nameElement.sendKeys(powerName)
})

Given('the power has a description of {string}', async function (powerDescription) {
  this.currentPower.description = powerDescription
  const descriptionElement = await this.browser.findElement(By.css('#TextAreaFormGroup-PowerEditor-Powers-PlotPoint-plotPointAdd-0-Description > div.ql-container.ql-snow > div.ql-editor'))
  await descriptionElement.clear()
  await descriptionElement.sendKeys(powerDescription)
})

Given('the power has power points of {int}', async function (powerPoints) {
  this.currentPower.powerPoints = powerPoints
  const powerPointsElement = await this.browser.findElement(By.id('FormControl-number-NumberFormGroup-PowerEditor-Powers-PlotPoint-plotPointAdd-0-PowerPoints'))
  await powerPointsElement.clear()
  await powerPointsElement.sendKeys(powerPoints.toString())
})

Given('the power has a range of {string}', async function (range) {
  this.currentPower.range = range
  const rangeElement = await this.browser.findElement(By.id('FormControl-text-TextFormGroup-PowerEditor-Powers-PlotPoint-plotPointAdd-0-Range'))
  await rangeElement.clear()
  await rangeElement.sendKeys(range)
})

Given('the power has a duration of {string}', async function (duration) {
  this.currentPower.duration = duration
  const durationElement = await this.browser.findElement(By.id('FormControl-text-TextFormGroup-PowerEditor-Powers-PlotPoint-plotPointAdd-0-Duration'))
  await durationElement.clear()
  await durationElement.sendKeys(duration)
})

Given('the power has trappings of {string}', async function (trappings) {
  this.currentPower.trappings = trappings
  const trappingsElement = await this.browser.findElement(By.id('FormControl-text-TextFormGroup-PowerEditor-Powers-PlotPoint-plotPointAdd-0-Trappings'))
  await trappingsElement.clear()
  await trappingsElement.sendKeys(trappings)
})

Given('I add a modifier with name {string} and description {string} and power point modifier of {int}', async function (modifierName, modifierDescription, powerPointModifier) {
  const modifier = {
    name: modifierName,
    description: modifierDescription,
    powerPointModifier: powerPointModifier
  }
  this.currentPower.modifiers.push(modifier)
  
  // Click add modifier button
  const addModifierButton = await this.browser.findElement(By.id('button-Modifiers-PowerEditor-Powers-PlotPoint-plotPointAdd-0'))
  await addModifierButton.click()
  
  const modifierIndex = this.currentPower.modifiers.length - 1
  
  // Fill in modifier fields
  const modifierNameElement = await this.browser.findElement(By.id(`FormControl-text-TextFormGroup-ModifierEditor-Modifiers-PowerEditor-Powers-PlotPoint-plotPointAdd-0-${modifierIndex}-Name`))
  await modifierNameElement.clear()
  await modifierNameElement.sendKeys(modifierName)
  
  const modifierDescElement = await this.browser.findElement(By.css(`#TextAreaFormGroup-ModifierEditor-Modifiers-PowerEditor-Powers-PlotPoint-plotPointAdd-0-${modifierIndex}-Description > div.ql-container.ql-snow > div.ql-editor`))
  await modifierDescElement.clear()
  await modifierDescElement.sendKeys(modifierDescription)
  
  const modifierPointsElement = await this.browser.findElement(By.id(`FormControl-number-NumberFormGroup-ModifierEditor-Modifiers-PowerEditor-Powers-PlotPoint-plotPointAdd-0-${modifierIndex}-PowerPointModifier`))
  await modifierPointsElement.clear()
  await modifierPointsElement.sendKeys(powerPointModifier.toString())
})

When('I save the power', async function () {
  // This would save just the power, but typically powers are saved with the plot point
  await this.savePlotPoint()
})

When('I try to save the power', async function () {
  // Attempt to save, expecting validation error
  await this.savePlotPoint()
})

Then('the power is saved to the plot point', async function () {
  // TODO: Verify via backend API
  console.log('TODO: Verify power is saved to plot point via backend API')
})

Then('the power has {int} modifiers', async function (modifierCount) {
  // TODO: Verify via backend API
  console.log(`TODO: Verify power has ${modifierCount} modifiers via backend API`)
})

Then('the operation fails', async function () {
  // Check for error message or validation failure
  sleep.sleep(1)
  const errorElements = await this.browser.findElements(By.className('error-message'))
  expect(errorElements.length).to.be.above(0)
})

// Moved to authentication_steps.js to avoid duplication
// Then('I see an error message {string}', async function (expectedErrorMessage) {
//   const errorElement = await this.browser.findElement(By.className('error-message'))
//   const actualErrorMessage = await errorElement.getText()
//   expect(actualErrorMessage).to.include(expectedErrorMessage)
// })

Given('I create a new plot point named {string}', async function (plotPointName) {
  const browser = this.browser
  await browser.get('http://localhost:3000')
  browser.wait(until.elementLocated(By.id('root')))
  await browser.findElement(By.id('button-addPlotPoint')).click()
  
  // Fill in plot point name
  const nameElement = await browser.findElement(By.id('FormControl-text-TextFormGroup-PlotPoint-plotPointAdd-Name'))
  await nameElement.clear()
  await nameElement.sendKeys(plotPointName)
  
  // Save the plot point
  const saveButton = await browser.findElement(By.id('button-save-PlotPoint-plotPointAdd'))
  await saveButton.click()
  
  sleep.sleep(2)
})

Then('the plot point should have the following default powers:', async function (dataTable) {
  // Navigate to the plot point edit page
  const plotPointLink = await this.browser.findElement(By.linkText(this.currentPlotPoint.name))
  await plotPointLink.click()
  
  // Navigate to powers tab
  const powersTab = await this.browser.findElement(By.id('NavItem-Navigation-PlotPoint-plotPointAdd-powers'))
  await powersTab.click()
  await this.browser.wait(until.elementLocated(By.id('Powers-PlotPoint-plotPointAdd')))
  
  // TODO: Verify default powers match the data table
  // This will need to check the powers list in the UI
  console.log('TODO: Verify default powers match expected list')
})

// Helper method
async function savePlotPoint() {
  const saveButton = await this.browser.findElement(By.id('button-save-PlotPoint-plotPointAdd'))
  await saveButton.click()
}