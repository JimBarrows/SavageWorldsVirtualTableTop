import '@babel/polyfill'
import {API, graphqlOperation} from "aws-amplify"
import {After, Before, Status} from 'cucumber'
import {writeFile}             from 'fs'
import {Builder}               from 'selenium-webdriver'
import * as mutations          from '../../src/graphql/mutations'
import * as queries            from '../../src/graphql/queries'

Before(async function () {
	this.browser = await new Builder()
		.forBrowser('chrome')
		.build()
	await this.browser.manage().setTimeouts({implicit: 45 * 1000, pageLoad: 45 * 1000})
})

After(async function (scenario) {
	const browser               = this.browser
	const plotPointListResponse = await API.graphql(graphqlOperation(queries.listPlotPoints))
	plotPointListResponse.data.listPlotPoints.items.filter(plotPoint => plotPoint.name === this.expected_plot_point.name).forEach(async plotPoint => await API.graphql(graphqlOperation(mutations.deletePlotPoint, {input: {id: plotPoint.id}})).catch(e => console.log(e)))

	if (scenario.result.status === Status.FAILED) {
		const image = await browser.takeScreenshot()
		writeFile('out.png', image, 'base64', function (err) {
			console.log(err)
		})
	}
	await browser.close()
	await browser.quit()
})
