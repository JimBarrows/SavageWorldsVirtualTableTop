import PropTypes from 'prop-types'
import React from 'react'
import AircraftEditor from '../editors/AircraftEditor'
import EditorList from './EditorList'

export default class AircraftEditorList extends React.Component {

	static propTypes = {
		id            : PropTypes.string.isRequired,
		aircraft      : PropTypes.array.isRequired,
		aircraftChange: PropTypes.func.isRequired
	}

	static defaultProps = {
		item: {
			acceleration: 1,
			armor       : 0,
			climb       : 0,
			crew        : 1,
			description : ' ',
			era         : ' ',
			kind        : ' ',
			maximumCost : 2,
			minimumCost : 1,
			name        : ' ',
			note        : ' ',
			passengers  : 0,
			topSpeed    : 1,
			toughness   : 2
		}
	}

	render() {
		let component_id = `AircraftEditorList-${this.props.id}`
		return (
			<div id={component_id}>
				<EditorList
					emptyItem={this.props.item}
					id={component_id}
					list={this.props.aircraft}
					onChange={this.props.aircraftChange}
					title={'Aircraft'}>
					<AircraftEditor item={this.props.item} onChange={e => console.log(e)}/>
				</EditorList>
			</div>
		)
	}
}

