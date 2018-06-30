import PropTypes from 'prop-types'
import React from 'react'
import AircraftEditor from '../editors/AircraftEditor'
import EditorList from './EditorList'

export default class AircraftEditorList extends React.Component {

	static propTypes = {
		id            : PropTypes.string.isRequired,
		aircraft      : PropTypes.array.isRequired,
		aircraftChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'AircraftEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name        : ' ',
								description : ' ',
								acceleration: 1,
								topSpeed    : 1,
								toughness   : 2,
								armor       : 1,
								minimumCost : 1,
								maximumCost : 2,
								notes       : ' '
							})}
							id={'aircraftEditorList'}
							list={this.props.aircraft}
							onChange={this.props.aircraftChange}
							title={'Aircraft'}>
						<AircraftEditor/>
					</EditorList>
				</div>
		);
	}
}

