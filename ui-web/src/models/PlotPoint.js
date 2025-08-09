import BasicRules from './BasicRules'
import Powers from './Powers'
import Skills from './Skills'

export default class PlotPoint {
	arcaneBackgrounds          = []
	basicRules                 = new BasicRules()
	beasts                     = []
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
