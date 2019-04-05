import {array, shape, string} from 'prop-types'
import BasicRules             from './BasicRules'
import Gear                   from './Gear'

export default shape({
											 airVehicles               : array,
											 ammunition                : array,
											 arcaneBackgrounds         : array,
											 armor                     : array,
											 basicRules                : BasicRules.isRequired,
											 beasts                    : array,
											 characters                : array,
											 description               : string,
											 edges                     : array,
											 gear                      : Gear,
											 gearEras                  : array,
											 gearKinds                 : array,
											 groundVehicles            : array,
											 handWeapons               : array,
											 hindrances                : array,
											 mundaneItems              : array,
											 name                      : string.isRequired,
											 powers                    : array,
											 races                     : array,
											 rangedWeapons             : array,
											 settingRules              : array,
											 skills                    : array,
											 specialWeapons            : array,
											 vehicleAndAtMountedWeapons: array,
											 waterVehicles             : array,
										 })
