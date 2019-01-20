import * as PropTypes from 'prop-types'
import React          from 'react'
import {Link}         from 'react-router-dom'

export default class PlotPointList extends React.Component {

	static propTypes = {
		id        : PropTypes.string.isRequired,
		plotPoints: PropTypes.arrayOf(PropTypes.shape({

																										basicRules : PropTypes.arrayOf(PropTypes.shape({
																																																		 maximumAttributePoints: PropTypes.number.isRequired,
																																																		 maximumMajorHindrances: PropTypes.number.isRequired,
																																																		 maximumMinorHindrances: PropTypes.number.isRequired,
																																																		 maximumSkillPoints    : PropTypes.number.isRequired
																																																	 })).isRequired,
																										beasts     : PropTypes.array,
																										description: PropTypes.string.isRequired,
																										edges      : PropTypes.array,
																										gear       : PropTypes.shape({
																																									 aircraft               : PropTypes.array,
																																									 ammunition             : PropTypes.array,
																																									 armor                  : PropTypes.array,
																																									 groundVehicles         : PropTypes.array,
																																									 handWeapons            : PropTypes.array,
																																									 mundaneItems           : PropTypes.array,
																																									 rangedWeapons          : PropTypes.array,
																																									 specialWeapons         : PropTypes.array,
																																									 vehicleMountedAndAtGuns: PropTypes.array,
																																									 watercraft             : PropTypes.array
																																								 }).isRequired,

																										hindrances  : PropTypes.array,
																										name        : PropTypes.string.isRequired,
																										powers      : PropTypes.shape({
																																										arcaneBackgrounds  : PropTypes.array,
																																										powers             : PropTypes.array,
																																										trappingsAndEffects: PropTypes.array
																																									}),
																										races       : PropTypes.array,
																										settingRules: PropTypes.array,
																										skills      : PropTypes.array
																									}
		))
	}
	static
	defaultProps     = {
		plotPoints: []
	}

	render () {
		return (
			<div id={'PlotPointListComponent'} >
				<table className={'table'} >
					<thead >
						<tr >
							<th >Name</th >
							<th >Description</th >
						</tr >
					</thead >
					<tbody >
						{
							this.props.plotPoints.map((pp, index) =>
																					<tr id={'plotPoint_' + index} key={index} >
																						<td >{pp.name}<Link to={`/plotPointEditor/${pp.name}`} >Edit</Link ></td >
																						<td >{pp.description}</td >
																					</tr >)
						}
					</tbody >
				</table >

			</div >
		)
	}
}

