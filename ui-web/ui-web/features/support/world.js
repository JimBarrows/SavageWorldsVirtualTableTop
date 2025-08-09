import {setWorldConstructor} from 'cucumber'
import PlotPoint             from './PlotPoint'

global.fetch = require('node-fetch')

function CustomWorld () {
	// Authentication will be handled by your backend implementation
	this.expected_plot_point = new PlotPoint()
	this.credentials         = {
		username: '',
		password: ''
	}
	this.signupData          = {}
}

setWorldConstructor(CustomWorld)
