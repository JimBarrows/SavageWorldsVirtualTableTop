import {Given, Then, When} from 'cucumber'
import {By, until}         from 'selenium-webdriver'

Given('I want to add a plot point', async function () {
	const browser = this.browser
	await browser.get('http://localhost:3000')
	browser.wait(until.elementLocated(By.id('root')))
	await browser.findElement(By.id('button-addPlotPoint')).click()
})

Given('a plot point name of {string}', async function (plot_point_name) {
	this.expected_plot_point.name = plot_point_name
	const browser                 = this.browser
	await browser.findElement(By.id('FormControl-text-TextFormGroup-PlotPoint-plotPointForm-Name')).sendKeys(this.expected_plot_point.name)
})

Given('a plot point description of {string}', async function (description) {
	this.expected_plot_point.description = description
	const browser                        = this.browser
	await browser.findElement(By.id('FormControl-text-TextFormGroup-PlotPoint-plotPointForm-Name')).sendKeys(this.expected_plot_point.description)
})

When('I save the plot point', async function () {
	const browser = this.browser
	await browser.findElement(By.id('button-save-PlotPoint-plotPointForm')).click()
})

Then('the operation is successful', function () {
	// Write code here that turns the phrase above into concrete actions
	return 'pending'
})

Then('the plot point is in the data store', function () {
	// Write code here that turns the phrase above into concrete actions
	return 'pending'
})

Then('I can continue to edit the plot point', function () {
	// Write code here that turns the phrase above into concrete actions
	return 'pending'
})
