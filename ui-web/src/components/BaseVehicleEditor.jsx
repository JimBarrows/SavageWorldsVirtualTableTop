import React from 'react';
import BaseEditor from './BaseEditor';
import NumberFormGroup from './NumberFormGroup';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class BaseVehicleEditor extends React.Component {

	accelerationChange = e => this.props.onChange(Object.assign({}, this.props.item, {acceleration: parseInt(e.target.value, 10)}), this.props.index);
	armorChange        = e => this.props.onChange(Object.assign({}, this.props.item, {armor: parseInt(e.target.value, 10)}), this.props.index);
	crewChange         = e => this.props.onChange(Object.assign({}, this.props.item, {crew: parseInt(e.target.value, 10)}), this.props.index);
	descriptionChange  = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	eraChange          = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
	kindChange         = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
	maximumCostChange  = e => this.props.onChange(Object.assign({}, this.props.item, {maximumCost: parseInt(e.target.value, 10)}), this.props.index);
	minimumCostChange  = e => this.props.onChange(Object.assign({}, this.props.item, {minimumCost: parseInt(e.target.value, 10)}), this.props.index);
	nameChange         = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	noteChange         = e => this.props.onChange(Object.assign({}, this.props.item, {notes: e.target.value}), this.props.index);
	onDelete           = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	passengersChange   = e => this.props.onChange(Object.assign({}, this.props.item, {passengers: parseInt(e.target.value, 10)}), this.props.index);
	topSpeedChange     = e => this.props.onChange(Object.assign({}, this.props.item, {topSpeed: parseInt(e.target.value, 10)}), this.props.index);
	toughnessChange    = e => this.props.onChange(Object.assign({}, this.props.item, {toughness: parseInt(e.target.value, 10)}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='baseVehicleName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<TextAreaFormGroup id={"baseVehicleDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
					<TextAreaFormGroup id={"baseVehicleNote"} label="Notes" onChange={this.noteChange}
					                   value={this.props.item.notes}/>
					<TextFormGroup id={'baseVehicleEra'} label={'Era'} onChange={this.eraChange} required={true}
					               value={this.props.item.era}/>
					<TextFormGroup id={'baseVehicleKind'} label={'Kind'} onChange={this.kindChange} required={true}
					               value={this.props.item.kind}/>
					<NumberFormGroup id={'baseVehicleAcceleration'} label={'Acceleration'} onChange={this.accelerationChange}
					                 required={true}
					                 value={this.props.item.acceleration}/>
					<NumberFormGroup id={'baseVehicleTopSpeed'} label={'Top Speed'} onChange={this.topSpeedChange} required={true}
					                 value={this.props.item.topSpeed}/>
					<NumberFormGroup id={'baseVehicleToughness'} label={'Toughness'} onChange={this.toughnessChange}
					                 required={true}
					                 value={this.props.item.toughness}/>
					<NumberFormGroup id={'baseVehicleArmor'} label={'Armor'} onChange={this.armorChange} required={true}
					                 value={this.props.item.armor}/>
					<NumberFormGroup id={'baseVehicleCrew'} label={'Crew'} onChange={this.crewChange} required={true}
					                 value={this.props.item.crew}/>
					<NumberFormGroup id={'baseVehiclePassengers'} label={'Passengers'} onChange={this.passengersChange}
					                 required={true}
					                 value={this.props.item.passengers}/>
					<NumberFormGroup id={'baseVehicleMinimumCostChange'} label={'Minimum Cost'} onChange={this.minimumCostChange}
					                 required={true}
					                 value={this.props.item.minimumCost}/>
					<NumberFormGroup id={'baseVehicleMaximumCostChange'} label={'Maximum Cost'} onChange={this.maximumCostChange}
					                 required={true}
					                 value={this.props.item.maximumCost}/>

					{this.additionalFields ? this.additionalFields() : ''}
				</BaseEditor>
		);
	}
}
