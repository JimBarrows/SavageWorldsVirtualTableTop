import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import BaseEditor from './BaseEditor'

export default class VehicleMountedAndAtGunsEditor extends React.Component {

	delete                = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	descriptionChange     = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	eraChange             = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
	kindChange            = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
	longRangeChange       = e => this.props.onChange(Object.assign({}, this.props.item, {longRange: parseInt(e.target.value, 10)}), this.props.index);
	mediumRangeChange     = e => this.props.onChange(Object.assign({}, this.props.item, {mediumRange: parseInt(e.target.value, 10)}), this.props.index);
	nameChange            = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	noteChange            = e => this.props.onChange(Object.assign({}, this.props.item, {note: e.target.value}), this.props.index)
	rateOfFireChange      = e => this.props.onChange(Object.assign({}, this.props.item, {rateOfFire: parseInt(e.target.value, 10)}), this.props.index);
	shortRangeChange      = e => this.props.onChange(Object.assign({}, this.props.item, {shortRange: parseInt(e.target.value, 10)}), this.props.index);
	apDamageChange        = e => this.props.onChange(Object.assign({}, this.props.item, {apDamage: e.target.value}), this.props.index);
	apArmorPiercingChange = e => this.props.onChange(Object.assign({}, this.props.item, {apArmorPiercing: parseInt(e.target.value, 10)}), this.props.index);
	heDamageChange        = e => this.props.onChange(Object.assign({}, this.props.item, {heDamage: e.target.value}), this.props.index);
	heArmorPiercingChange = e => this.props.onChange(Object.assign({}, this.props.item, {heArmorPiercing: parseInt(e.target.value, 10)}), this.props.index);
	heBurstTemplateChange = e => this.props.onChange(Object.assign({}, this.props.item, {heBurstTemplate: e.target.value}), this.props.index);

	render() {
		return <BaseEditor id={this.props.id} onDelete={this.delete}>
			<TextFormGroup id='mundaneItemName'
			               label='Name'
			               onChange={this.nameChange}
			               required={true}
			               value={this.props.item.name}/>
			<TextAreaFormGroup id={"mundaneItemDescription"}
			                   label="Description"
			                   onChange={this.descriptionChange}
			                   value={this.props.item.description}/>
			<NumberFormGroup id={'vehicleMountedAndAtGunsShort'}
			                 label={'Short Range'}
			                 onChange={this.shortRangeChange}
			                 required={true}
			                 value={this.props.item.shortRange}/>
			<NumberFormGroup id={'vehicleMountedAndAtGunsMedium'}
			                 label={'Medium Range'}
			                 onChange={this.mediumRangeChange}
			                 required={true}
			                 value={this.props.item.mediumRange}/>
			<NumberFormGroup id={'vehicleMountedAndAtGunsLong'}
			                 label={'Long Range'}
			                 onChange={this.longRangeChange}
			                 required={true}
			                 value={this.props.item.longRange}/>
			<TextFormGroup id={'vehicleMountedAndAtGunsApDamage'}
			               label={'AP Damage'}
			               onChange={this.apDamageChange}
			               required={true}
			               value={this.props.item.apDamage}/>
			<NumberFormGroup id={'vehicleMountedAndAtGunsApArmorPiercing'}
			                 label={'AP Armor Piercing'}
			                 onChange={this.apArmorPiercingChange}
			                 required={true}
			                 value={this.props.item.apArmorPiercing}/>
			<TextFormGroup id={'vehicleMountedAndAtGunsHeDamage'}
			               label={'HE Damage'}
			               onChange={this.heDamageChange}
			               value={this.props.item.heDamage}/>
			<TextFormGroup id={'vehicleMountedAndAtGunsHeBurstTemplate'}
			               label={'HE Burst Template'}
			               onChange={this.heBurstTemplateChange}
			               value={this.props.item.heBurstTemplate}/>
			<NumberFormGroup id={'vehicleMountedAndAtGunsHeArmorPiercing'}
			                 label={'HE Armor Piercing'}
			                 onChange={this.heArmorPiercingChange}
			                 value={this.props.item.heArmorPiercing}/>
			<NumberFormGroup id={'vehicleMountedAndAtGunsRateOfFire'}
			                 label={'Rate of Fire'}
			                 onChange={this.rateOfFireChange}
			                 required={true}
			                 value={this.props.item.rateOfFire}/>
			<TextAreaFormGroup id={"vehicleMountedAndAtGunsNote"}
			                   label="Note"
			                   onChange={this.noteChange}
			                   value={this.props.item.note}/>
			<TextFormGroup id={'vehicleMountedAndAtGunsEra'}
			               label={'Era'}
			               onChange={this.eraChange}
			               required={true}
			               value={this.props.item.era}/>
			<TextFormGroup id={'vehicleMountedAndAtGunsKind'}
			               label={'Kind'}
			               onChange={this.kindChange}
			               required={true}
			               value={this.props.item.kind}/>
		</BaseEditor>;
	}
}
