import {array, shape, string} from 'prop-types'
import BasicRules             from './BasicRules'
import Gear                   from './Gear'
import Powers                 from './Powers'

export default shape({
											 basicRules  : BasicRules.isRequired,
											 beasts      : array.isRequired,
											 description : string.isRequired,
											 edges       : array.isRequired,
											 gear        : Gear.isRequired,
											 hindrances  : array.isRequired,
											 name        : string.isRequired,
											 powers      : Powers.isRequired,
											 races       : array.isRequired,
											 settingRules: array.isRequired,
											 skills      : array.isRequired
										 })
