import Amplify, {Auth}       from 'aws-amplify'
import {setWorldConstructor} from 'cucumber'
import awsmobile             from '../../src/aws-exports'
import PlotPoint             from './PlotPoint'

global.fetch = require('node-fetch')

async function configure_amplify () {
	Amplify.configure(awsmobile)
	return await Auth.signIn({
														 username: 'ChesterTester',
														 password: 'ChesterTester1!'
													 })
}

function CustomWorld () {

	this.auth                = configure_amplify()
	this.expected_plot_point = new PlotPoint()
	this.credentials         = {
		username: '',
		password: ''
	}
}

setWorldConstructor(CustomWorld)
