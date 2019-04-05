import * as PropTypes from 'prop-types'
import React          from 'react'
import {Link}         from 'react-router-dom'
import PlotPoint      from '../../../propTypes/PlotPoint'

export default class PlotPointList extends React.Component {

	static propTypes = {
		id        : PropTypes.string.isRequired,
		plotPoints: PropTypes.arrayOf(PlotPoint)
	}

	static defaultProps = {
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

