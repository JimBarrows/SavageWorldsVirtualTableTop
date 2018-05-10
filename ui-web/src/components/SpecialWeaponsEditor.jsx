import PropTypes from 'prop-types';
import React from 'react';
import MundaneItemEditor from './MundaneItemEditor';
import NumberFormGroup from './NumberFormGroup';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class SpecialWeaponsEditor extends MundaneItemEditor {

	static propTypes = {
		id      : PropTypes.string.isRequired,
		item    : PropTypes.object.isRequired,
		onChange: PropTypes.func.isRequired
	};

	onApChange              = e => this.props.onChange(Object.assign({}, this.props.item, {armorPiercing: parseInt(e.target.value, 10)}), this.props.index);
	onDamageChange          = e => this.props.onChange(Object.assign({}, this.props.item, {damage: e.target.value}), this.props.index);
	onEraChange             = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
	onKindChange            = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
	onLongRangeChange       = e => this.props.onChange(Object.assign({}, this.props.item, {longRange: parseInt(e.target.value, 10)}), this.props.index);
	onMediumRangeChange     = e => this.props.onChange(Object.assign({}, this.props.item, {mediumRange: parseInt(e.target.value, 10)}), this.props.index);
	onMinimumStrengthChange = e => this.props.onChange(Object.assign({}, this.props.item, {minimumStrength: e.target.value}), this.props.index);
	onNoteChange            = e => this.props.onChange(Object.assign({}, this.props.item, {notes: e.target.value}), this.props.index);
	onRateOfFireChange      = e => this.props.onChange(Object.assign({}, this.props.item, {rateOfFire: parseInt(e.target.value, 10)}), this.props.index);
	onShortRangeChange      = e => this.props.onChange(Object.assign({}, this.props.item, {shortRange: parseInt(e.target.value, 10)}), this.props.index);
	onShotsChange           = e => this.props.onChange(Object.assign({}, this.props.item, {shots: parseInt(e.target.value, 10)}), this.props.index);
	onBurstTemplateChange   = e => this.props.onChange(Object.assign({}, this.props.item, {burstTemplate: e.target.value}), this.props.index);

	additionalFields = () => <div id={'SpecialWeaponsEditorComponent_' + this.props.id}>
		<NumberFormGroup id={'specialWeaponsShort'} label={'Short Range'} onChange={this.onShortRangeChange}
		                 required={true} value={this.props.item.shortRange}/>
		<NumberFormGroup id={'specialWeaponsMedium'} label={'Medium Range'} onChange={this.onMediumRangeChange}
		                 required={true} value={this.props.item.mediumRange}/>
		<NumberFormGroup id={'specialWeaponsLong'} label={'Long Range'} onChange={this.onLongRangeChange}
		                 required={true} value={this.props.item.longRange}/>
		<TextFormGroup id={'specialWeaponsDamage'} label={'Damage'} onChange={this.onDamageChange} required={true}
		               value={this.props.item.damage}/>
		<NumberFormGroup id={'specialWeaponsRateOfFire'} label={'Rate of Fire'} onChange={this.onRateOfFireChange}
		                 required={true} value={this.props.item.rateOfFire}/>
		<NumberFormGroup id={'specialWeaponsAp'} label={'Armor Piercing'} onChange={this.onApChange}
		                 required={true} value={this.props.item.armorPiercing}/>
		<NumberFormGroup id={'specialWeaponsShots'} label={'Shots'} onChange={this.onShotsChange}
		                 required={true} value={this.props.item.shots}/>
		<TextFormGroup id={'specialWeaponsMinimumStrength'} label={'Minimum Strength'}
		               onChange={this.onMinimumStrengthChange}
		               required={true} value={this.props.item.minimumStrength}/>
		<TextFormGroup id={'vehicleMountedAndAtGunsBurstTemplate'} label={'Burst Template'}
		               onChange={this.onBurstTemplateChange}
		               required={true} value={this.props.item.heBurstTemplate}/>
		<TextFormGroup id={'specialWeaponsEra'} label={'Era'} onChange={this.onEraChange} required={true}
		               value={this.props.item.era}/>
		<TextFormGroup id={'specialWeaponsKind'} label={'Kind'} onChange={this.onKindChange} required={true}
		               value={this.props.item.kind}/>
		<TextAreaFormGroup id={"specialWeaponsNote"}
		                   label="Notes"
		                   onChange={this.onNoteChange}
		                   value={this.props.item.notes}/>
	</div>;
}
