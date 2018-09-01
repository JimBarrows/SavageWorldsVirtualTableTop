import PropTypes from 'prop-types'
import React from 'react'
import BaseVehicleEditor from '../editors/BaseVehicleEditor'
import EditorList from './EditorList'

export default class GroundVehicles extends React.Component {

	static propTypes = {
		id                  : PropTypes.string.isRequired,
		groundVehicles      : PropTypes.array.isRequired,
		groundVehiclesChange: PropTypes.func.isRequired
	}

	static defaultProps = {
		item: {
			acceleration: 1,
			armor       : 0,
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
		let component_id = `GroundVehiclesEditorListComponent-${this.props.id}`
		return (
			<div id={component_id}>
				<EditorList
					emptyItem={this.props.item}
					id={component_id}
					list={this.props.groundVehicles}
					onChange={this.props.groundVehiclesChange}
					title={'Ground Vehicles'}>
					<BaseVehicleEditor item={this.props.item} onChange={e => console.log(e)}/>
				</EditorList>
			</div>
		)
	}
}

