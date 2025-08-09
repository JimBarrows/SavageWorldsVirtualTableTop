import BasicRules from './BasicRules'
import Powers from './Powers'
import Skills from './Skills'
import Beasts from './Beasts'

export default class PlotPoint {
	arcaneBackgrounds          = []
	basicRules                 = new BasicRules()
	beasts                     = Beasts.getStandardBeasts()
	characters                 = []
	description                = 'This is a description'
	edges                      = []
	ammunition                 = []
	armor                      = []
	mundaneItems               = []
	handWeapons                = []
	rangedWeapons              = []
	specialWeapons             = []
	vehicleAndAtMountedWeapons = []
	gearEras                   = []
	gearKinds                  = []
	hindrances                 = []
	id                         = ''
	name                       = ''
	powers                     = Powers.getDefaultPowers()
	races                      = []
	settingRules               = []
	skills                     = Skills.getDefaultSkills()
	airVehicles                = []
	waterVehicles              = []
	groundVehicles             = []
}
