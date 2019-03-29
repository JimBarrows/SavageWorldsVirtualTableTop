import BasicRules from './BasicRules'
import Gear       from './Gear'
import Powers     from './Powers'

export default class PlotPoint {
	basicRules   = new BasicRules()
	beasts       = []
	characters   = []
	description  = ''
	edges        = []
	gear         = new Gear()
	hindrances   = []
	name         = ''
	powers       = new Powers()
	races        = []
	settingRules = []
	skills       = []

	addBeast     = beast => this.beasts.push(beast)
	addCharacter = character => this.characters.push(character)

}
