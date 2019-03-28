import BasicRules from './BasicRules'
import Gear       from './Gear'
import Powers     from './Powers'

export default class PlotPoint {
	basicRules   = new BasicRules()
	beasts       = []
	description  = ''
	edges        = []
	gear         = new Gear()
	hindrances   = []
	name         = ''
	powers       = new Powers()
	races        = []
	settingRules = []
	skills       = []
}
