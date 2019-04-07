import {API, graphqlOperation} from "aws-amplify"
import {Given, Then, When}     from 'cucumber'
import {By, until}             from 'selenium-webdriver'
import sleep                   from 'sleep'
import * as queries            from '../../src/graphql/queries'


Given('I want to add a plot point', async function () {
	const browser = this.browser
	await browser.get('http://localhost:3000')
	browser.wait(until.elementLocated(By.id('root')))
	await browser.findElement(By.id('button-addPlotPoint')).click()
})

Given('a plot point name of {string}', async function (plot_point_name) {
	this.expected_plot_point.name = plot_point_name
	const element                 = await this.browser.findElement(By.id('FormControl-text-TextFormGroup-PlotPoint-plotPointAdd-Name'))
	await element.clear()
	await element.sendKeys(this.expected_plot_point.name)
})

Given('a plot point description of {string}', async function (plot_point_description) {
	this.expected_plot_point.description = plot_point_description
	const element                        = await this.browser.findElement(By.css('#TextAreaFormGroup-PlotPoint-plotPointAdd-Description > div.ql-container.ql-snow > div.ql-editor'))
	await element.clear()
	await element.sendKeys(this.expected_plot_point.description)
})

Given('I want the maximum attribute points to be {int}', async function (maximum_attribute_points) {
	this.expected_plot_point.basic_rules.maximumAttributePoints = maximum_attribute_points
	const element                                               = await this.browser.findElement(By.id('FormControl-number-NumberFormGroup-BasicRulesComponent_-PlotPoint-plotPointAdd-MaximumAttributePoints'))
	await element.clear()
	await element.sendKeys(this.expected_plot_point.basic_rules.maximumAttributePoints)
})

Given('I want the maximum number of major hindrances to be {int}', async function (maximum_number_of_major_hindrances) {
	this.expected_plot_point.basic_rules.maximumNumberOfMajorHindrances = maximum_number_of_major_hindrances
	const element                                                       = await this.browser.findElement(By.id('FormControl-number-NumberFormGroup-BasicRulesComponent_-PlotPoint-plotPointAdd-MaximumMajorHindrances'))
	await element.clear()
	await element.sendKeys(this.expected_plot_point.basic_rules.maximumNumberOfMajorHindrances)
})

Given('I want the maximum number of minor hindrances to be {int}', async function (minimum_number_of_major_hindrances) {
	this.expected_plot_point.basic_rules.minimumNumberOfMajorHindrances = minimum_number_of_major_hindrances
	const element                                                       = await this.browser.findElement(By.id('FormControl-number-NumberFormGroup-BasicRulesComponent_-PlotPoint-plotPointAdd-MaximumMinorHindrances'))
	await element.clear()
	await element.sendKeys(this.expected_plot_point.basic_rules.minimumNumberOfMajorHindrances)
})


Given('I want the maximum skill points to be {int}', async function (maximumSkillPoints) {
	this.expected_plot_point.basic_rules.maximumSkillPoints = maximumSkillPoints
	const element                                           = await this.browser.findElement(By.id('FormControl-number-NumberFormGroup-BasicRulesComponent_-PlotPoint-plotPointAdd-MaximumSkillPoints'))
	await element.clear()
	await element.sendKeys(this.expected_plot_point.basic_rules.maximumSkillPoints)
})


When('I save the plot point', async function () {
	const browser = this.browser
	const button  = await browser.findElement(By.id('button-save-PlotPoint-plotPointAdd'))
	await button.click()
})

Then('the operation is successful', async function () {
	sleep.sleep(2)
	const elements = await this.browser.findElements(By.id('PageHeader-PlotPointListPage'))
	expect(elements.length).to.be.above(0)
})

Then('the plot point is in the data store', async function () {
	const plotPointListResponse = await API.graphql(graphqlOperation(queries.listPlotPoints))
	expect(plotPointListResponse.data.listPlotPoints.items.filter(plotPoint => plotPoint.name === this.expected_plot_point.name).length).to.be.equal(1)
})


Then('the maximum attribute points are {int}', async function (maximum_attribute_points) {
	const plotPointListResponse = await API.graphql(graphqlOperation(queries.listPlotPoints))
	const plotPoint             = plotPointListResponse.data.listPlotPoints.items.filter(plotPoint => plotPoint.name === this.expected_plot_point.name)[0]
	expect(plotPoint.basicRules.maximumAttributePoints).to.be.equal(maximum_attribute_points)
})

Then('the maximum number of major hindrances is {int}', async function (maximumNumberOfMajorHindrances) {
	const plotPointListResponse = await API.graphql(graphqlOperation(queries.listPlotPoints))
	const plotPoint             = plotPointListResponse.data.listPlotPoints.items.filter(plotPoint => plotPoint.name === this.expected_plot_point.name)[0]
	expect(plotPoint.basicRules.maximumMajorHindrances).to.be.equal(maximumNumberOfMajorHindrances)

})


Then('the maximum number of minor hindrances is {int}', async function (maximumNumberMinorHindrances) {
	const plotPointListResponse = await API.graphql(graphqlOperation(queries.listPlotPoints))
	const plotPoint             = plotPointListResponse.data.listPlotPoints.items.filter(plotPoint => plotPoint.name === this.expected_plot_point.name)[0]
	expect(plotPoint.basicRules.maximumMinorHindrances).to.be.equal(maximumNumberMinorHindrances)
})

Then('the maximum skill points is {int}', async function (maximumNumberSkillPoints) {
	const plotPointListResponse = await API.graphql(graphqlOperation(queries.listPlotPoints))
	const plotPoint             = plotPointListResponse.data.listPlotPoints.items.filter(plotPoint => plotPoint.name === this.expected_plot_point.name)[0]
	expect(plotPoint.basicRules.maximumSkillPoints).to.be.equal(maximumNumberSkillPoints)
})

