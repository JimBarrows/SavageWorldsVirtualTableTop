import {number, shape} from 'prop-types'

export default shape({
											 maximumAttributePoints: number.isRequired,
											 maximumMajorHindrances: number.isRequired,
											 maximumMinorHindrances: number.isRequired,
											 maximumSkillPoints    : number.isRequired
										 })
