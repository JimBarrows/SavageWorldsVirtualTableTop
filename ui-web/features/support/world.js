import {setWorldConstructor} from 'cucumber'
import PlotPoint             from './PlotPoint.js'
import Scene                 from './Scene.js'

global.fetch = require('node-fetch')

function CustomWorld () {
	// Authentication will be handled by your backend implementation
	this.expected_plot_point = new PlotPoint()
	this.expected_scene = new Scene()
	this.available_characters = []
	this.credentials         = {
		username: '',
		password: ''
	}
	this.signupData          = {}
}

setWorldConstructor(CustomWorld)
