import PropTypes from 'prop-types'
import React from 'react'
import RaceEditorList from '../lists/RaceEditorList'

export default class Race extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : PropTypes.string.isRequired,
		onChange : PropTypes.func.isRequired,
		plotPoint: PropTypes.object.isRequired
	}

	racesChange                   = races => this.props.onChange(Object.assign({}, this.props.plotPoint, {races}))

	render() {
		let {id, plotPoint}        = this.props
		let component_id = `Race-${id}`
		return (
			<div id={component_id}>
				<h1>Race</h1>
				<RaceEditorList id={component_id} races={plotPoint.races} racesChange={this.racesChange}/>
			</div>
		)
	}
}
