import PropTypes from 'prop-types';
import React from 'react';
import MundaneItemEditor from './MundaneItemEditor';
import NumberFormGroup from './NumberFormGroup';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class VehicleMountedAndAtGunsEditor extends MundaneItemEditor {

	static propTypes = {
		id      : PropTypes.string.isRequired,
		item    : PropTypes.object.isRequired,
		onChange: PropTypes.func.isRequired
	};

	onEraChange             = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
	onKindChange            = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
	onLongRangeChange       = e => this.props.onChange(Object.assign({}, this.props.item, {longRange: parseInt(e.target.value, 10)}), this.props.index);
	onMediumRangeChange     = e => this.props.onChange(Object.assign({}, this.props.item, {mediumRange: parseInt(e.target.value, 10)}), this.props.index);
	onMinimumStrengthChange = e => this.props.onChange(Object.assign({}, this.props.item, {minimumStrength: e.target.value}), this.props.index);
	onNoteChange            = e => this.props.onChange(Object.assign({}, this.props.item, {notes: e.target.value}), this.props.index);
	onRateOfFireChange      = e => this.props.onChange(Object.assign({}, this.props.item, {rateOfFire: parseInt(e.target.value, 10)}), this.props.index);
	onShortRangeChange      = e => this.props.onChange(Object.assign({}, this.props.item, {shortRange: parseInt(e.target.value, 10)}), this.props.index);
	onApDamageChange        = e => this.props.onChange(Object.assign({}, this.props.item, {apDamage: e.target.value}), this.props.index);
	onApArmorPiercingChange = e => this.props.onChange(Object.assign({}, this.props.item, {apArmorPiercing: parseInt(e.target.value, 10)}), this.props.index);
	onHeDamageChange        = e => this.props.onChange(Object.assign({}, this.props.item, {heDamage: e.target.value}), this.props.index);
	onHeArmorPiercingChange = e => this.props.onChange(Object.assign({}, this.props.item, {heArmorPiercing: parseInt(e.target.value, 10)}), this.props.index);
	onHeBurstTemplateChange = e => this.props.onChange(Object.assign({}, this.props.item, {heBurstTemplate: e.target.value}), this.props.index);

	additionalFields = () => <div id={'VehicleMountedAndAtGunsEditor_' + this.id}>
		<NumberFormGroup id={'vehicleMountedAndAtGunsShort'} label={'Short Range'} onChange={this.onShortRangeChange}
		                 required={true} value={this.props.item.shortRange}/>
		<NumberFormGroup id={'vehicleMountedAndAtGunsMedium'} label={'Medium Range'} onChange={this.onMediumRangeChange}
		                 required={true} value={this.props.item.mediumRange}/>
		<NumberFormGroup id={'vehicleMountedAndAtGunsLong'} label={'Long Range'} onChange={this.onLongRangeChange}
		                 required={true} value={this.props.item.longRange}/>
		<TextFormGroup id={'vehicleMountedAndAtGunsApDamage'} label={'AP Damage'} onChange={this.onApDamageChange}
		               required={true} value={this.props.item.apDamage}/>
		<NumberFormGroup id={'vehicleMountedAndAtGunsApArmorPiercing'} label={'AP Armor Piercing'}
		                 onChange={this.onApArmorPiercingChange} required={true} value={this.props.item.apArmorPiercing}/>
		<TextFormGroup id={'vehicleMountedAndAtGunsHeDamage'} label={'HE Damage'} onChange={this.onHeDamageChange}
		               required={true} value={this.props.item.heDamage}/>
		<TextFormGroup id={'vehicleMountedAndAtGunsHeBurstTemplate'} label={'HE Burst Template'}
		               onChange={this.onHeBurstTemplateChange}
		               required={true} value={this.props.item.heBurstTemplate}/>
		<NumberFormGroup id={'vehicleMountedAndAtGunsHeArmorPiercing'} label={'HE Armor Piercing'}
		                 onChange={this.onHeArmorPiercingChange} required={true} value={this.props.item.heArmorPiercing}/>
		<NumberFormGroup id={'vehicleMountedAndAtGunsRateOfFire'} label={'Rate of Fire'} onChange={this.onRateOfFireChange}
		                 required={true} value={this.props.item.rateOfFire}/>
		<TextAreaFormGroup id={"vehicleMountedAndAtGunsNote"} label="Notes" onChange={this.onNoteChange}
		                   value={this.props.item.notes}/>
		<TextFormGroup id={'vehicleMountedAndAtGunsEra'} label={'Era'} onChange={this.onEraChange} required={true}
		               value={this.props.item.era}/>
		<TextFormGroup id={'vehicleMountedAndAtGunsKind'} label={'Kind'} onChange={this.onKindChange} required={true}
		               value={this.props.item.kind}/>
	</div>;
}
