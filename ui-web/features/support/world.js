import {setWorldConstructor} from 'cucumber'
import PlotPoint             from './PlotPoint'

function CustomWorld () {

	this.expected_plot_point = new PlotPoint()
}

setWorldConstructor(CustomWorld)
