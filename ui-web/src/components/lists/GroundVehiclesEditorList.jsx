import PropTypes from 'prop-types'
import React from 'react'
import BaseVehicleEditor from '../editors/BaseVehicleEditor'
import EditorList from './EditorList'

export default class GroundVehiclesEditorList extends React.Component {

	static propTypes = {
		id                  : PropTypes.string.isRequired,
		groundVehicles      : PropTypes.array.isRequired,
		groundVehiclesChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'GroundVehiclesEditorListComponent_' + this.props.id}>
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
							id={'groundVehiclesEditorList'}
							list={this.props.groundVehicles}
							onChange={this.props.groundVehiclesChange}
							title={'Ground Vehicles'}>
						<BaseVehicleEditor/>
					</EditorList>
				</div>
		);
	}
}

