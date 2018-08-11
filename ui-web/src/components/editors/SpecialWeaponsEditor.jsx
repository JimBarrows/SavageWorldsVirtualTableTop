import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import BaseEditor from './BaseEditor'

export default class SpecialWeaponsEditor extends React.Component {

	onApChange              = e => this.props.onChange(Object.assign({}, this.props.item, {armorPiercing: parseInt(e.target.value, 10)}), this.props.index);
	onDamageChange          = e => this.props.onChange(Object.assign({}, this.props.item, {damage: e.target.value}), this.props.index);
	onEraChange             = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
	onKindChange            = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
	onLongRangeChange       = e => this.props.onChange(Object.assign({}, this.props.item, {longRange: parseInt(e.target.value, 10)}), this.props.index);
	onMediumRangeChange     = e => this.props.onChange(Object.assign({}, this.props.item, {mediumRange: parseInt(e.target.value, 10)}), this.props.index);
	onMinimumStrengthChange = e => this.props.onChange(Object.assign({}, this.props.item, {minimumStrength: e.target.value}), this.props.index);
	onNoteChange            = e => this.props.onChange(Object.assign({}, this.props.item, {note: e.target.value}), this.props.index)
	onRateOfFireChange      = e => this.props.onChange(Object.assign({}, this.props.item, {rateOfFire: parseInt(e.target.value, 10)}), this.props.index);
	onShortRangeChange      = e => this.props.onChange(Object.assign({}, this.props.item, {shortRange: parseInt(e.target.value, 10)}), this.props.index);
	onShotsChange           = e => this.props.onChange(Object.assign({}, this.props.item, {shots: parseInt(e.target.value, 10)}), this.props.index);
	onBurstTemplateChange   = e => this.props.onChange(Object.assign({}, this.props.item, {burstTemplate: e.target.value}), this.props.index);
	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	onDelete          = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};

	render() {
		return <BaseEditor id={this.props.id} onDelete={this.onDelete}>
			<TextFormGroup id='mundaneItemName' label='Name' onChange={this.nameChange} required={true}
			               value={this.props.item.name}/>
			<TextAreaFormGroup id={"mundaneItemDescription"}
			                   label="Description"
			                   onChange={this.descriptionChange}
			                   value={this.props.item.description}/>
			<NumberFormGroup id={'specialWeaponsShort'}
			                 label={'Short Range'}
			                 onChange={this.onShortRangeChange}
			                 required={true}
			                 value={this.props.item.shortRange}/>
			<NumberFormGroup id={'specialWeaponsMedium'}
			                 label={'Medium Range'}
			                 onChange={this.onMediumRangeChange}
			                 required={true}
			                 value={this.props.item.mediumRange}/>
			<NumberFormGroup id={'specialWeaponsLong'}
			                 label={'Long Range'}
			                 onChange={this.onLongRangeChange}
			                 required={true}
			                 value={this.props.item.longRange}/>
			<TextFormGroup id={'specialWeaponsDamage'}
			               label={'Damage'}
			               onChange={this.onDamageChange}
			               required={true}
			               value={this.props.item.damage}/>
			<NumberFormGroup id={'specialWeaponsRateOfFire'}
			                 label={'Rate of Fire'}
			                 onChange={this.onRateOfFireChange}
			                 required={true}
			                 value={this.props.item.rateOfFire}/>
			<NumberFormGroup id={'specialWeaponsAp'}
			                 label={'Armor Piercing'}
			                 onChange={this.onApChange}
			                 required={true}
			                 value={this.props.item.armorPiercing}/>
			<TextFormGroup id={'specialWeaponsMinimumStrength'}
			               label={'Minimum Strength'}
			               onChange={this.onMinimumStrengthChange}
			               value={this.props.item.minimumStrength}/>
			<TextFormGroup id={'vehicleMountedAndAtGunsBurstTemplate'} label={'Burst Template'}
			               onChange={this.onBurstTemplateChange}
			               value={this.props.item.heBurstTemplate}/>
			<TextFormGroup id={'specialWeaponsEra'} label={'Era'}
			               onChange={this.onEraChange}
			               required={true}
			               value={this.props.item.era}/>
			<TextFormGroup id={'specialWeaponsKind'}
			               label={'Kind'}
			               onChange={this.onKindChange}
			               required={true}
			               value={this.props.item.kind}/>
			<TextAreaFormGroup id={"specialWeaponsNote"}
			                   label="note"
			                   onChange={this.onNoteChange}
			                   value={this.props.item.note}/>
		</BaseEditor>;
	}
}
