import {NumberFormGroup} from 'bootstrap-react-components'
import PropTypes         from 'prop-types'
import React             from 'react'
import GearEditor        from '../gear/GearEditor'

export default class VehicleEditor extends GearEditor {

	static propTypes = {
		item    : PropTypes.shape({
			acceleration: PropTypes.number,
			armor       : PropTypes.number,
			crew        : PropTypes.number,
			description : PropTypes.string,
			era         : PropTypes.string,
			kind        : PropTypes.string,
			maximumCost : PropTypes.number,
			minimumCost : PropTypes.number,
			name        : PropTypes.string,
			note        : PropTypes.string,
			passengers  : PropTypes.number,
			topSpeed    : PropTypes.number,
			toughness   : PropTypes.number
		}).isRequired,
		onChange: PropTypes.func.isRequired
	}

	accelerationChange = e => this.props.onChange(Object.assign({}, this.props.item, {acceleration: parseInt(e.target.value, 10)}), this.props.index)
	armorChange        = e => this.props.onChange(Object.assign({}, this.props.item, {armor: parseInt(e.target.value, 10)}), this.props.index)
	crewChange         = e => this.props.onChange(Object.assign({}, this.props.item, {crew: parseInt(e.target.value, 10)}), this.props.index)
	maximumCostChange  = e => this.props.onChange(Object.assign({}, this.props.item, {maximumCost: parseInt(e.target.value, 10)}), this.props.index)
	minimumCostChange  = e => this.props.onChange(Object.assign({}, this.props.item, {minimumCost: parseInt(e.target.value, 10)}), this.props.index)
	passengersChange   = e => this.props.onChange(Object.assign({}, this.props.item, {passengers: parseInt(e.target.value, 10)}), this.props.index)
	topSpeedChange     = e => this.props.onChange(Object.assign({}, this.props.item, {topSpeed: parseInt(e.target.value, 10)}), this.props.index)
	toughnessChange    = e => this.props.onChange(Object.assign({}, this.props.item, {toughness: parseInt(e.target.value, 10)}), this.props.index)

	additionalFields = () => <div>
		<NumberFormGroup id={'BaseVehicle-Acceleration'} label={'Acceleration'} onChange={this.accelerationChange}
		                 required={true}
		                 value={this.props.item.acceleration}/>
		<NumberFormGroup id={'BaseVehicle-TopSpeed'} label={'Top Speed'} onChange={this.topSpeedChange} required={true}
		                 value={this.props.item.topSpeed}/>
		<NumberFormGroup id={'BaseVehicle-Toughness'} label={'Toughness'} onChange={this.toughnessChange}
		                 required={true}
		                 value={this.props.item.toughness}/>
		<NumberFormGroup id={'BaseVehicle-Armor'} label={'Armor'} onChange={this.armorChange} required={true}
		                 value={this.props.item.armor}/>
		<NumberFormGroup id={'BaseVehicle-Crew'} label={'Crew'} onChange={this.crewChange} required={true}
		                 value={this.props.item.crew}/>
		<NumberFormGroup id={'BaseVehicle-Passengers'} label={'Passengers'} onChange={this.passengersChange}
		                 required={true}
		                 value={this.props.item.passengers}/>
		<NumberFormGroup id={'BaseVehicle-MinimumCostChange'} label={'Minimum Cost'} onChange={this.minimumCostChange}
		                 required={true}
		                 value={this.props.item.minimumCost}/>
		<NumberFormGroup id={'BaseVehicle-MaximumCostChange'} label={'Maximum Cost'} onChange={this.maximumCostChange}
		                 required={true}
		                 value={this.props.item.maximumCost}/>
		{this.additionalAdditionalFields ? this.additionalAdditionalFields() : ''}
	</div>

}
