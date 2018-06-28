import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import MundaneItemEditor from './MundaneItemEditor'

export default class RangedWeaponEditor extends MundaneItemEditor {

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

	additionalFields = () => <div id={'RangedWeaponEditorComponent_' + this.props.id}>
		<NumberFormGroup id={'rangedWeaponShort'} label={'Short Range'} onChange={this.onShortRangeChange}
		                 required={true} value={this.props.item.shortRange}/>
		<NumberFormGroup id={'rangedWeaponMedium'} label={'Medium Range'} onChange={this.onMediumRangeChange}
		                 required={true} value={this.props.item.mediumRange}/>
		<NumberFormGroup id={'rangedWeaponLong'} label={'Long Range'} onChange={this.onLongRangeChange}
		                 required={true} value={this.props.item.longRange}/>
		<TextFormGroup id={'rangedWeaponDamage'} label={'Damage'} onChange={this.onDamageChange} required={true}
		               value={this.props.item.damage}/>
		<NumberFormGroup id={'rangedWeaponRateOfFire'} label={'Rate of Fire'} onChange={this.onRateOfFireChange}
		                 required={true} value={this.props.item.rateOfFire}/>
		<NumberFormGroup id={'rangedWeaponShots'} label={'Shots'} onChange={this.onShotsChange}
		                 required={true} value={this.props.item.shots}/>
		<TextFormGroup id={'rangedWeaponMinimumStrength'} label={'Minimum Strength'}
		               onChange={this.onMinimumStrengthChange}
		               required={true} value={this.props.item.minimumStrength}/>
		<TextFormGroup id={'rangedWeaponEra'} label={'Era'} onChange={this.onEraChange} required={true}
		               value={this.props.item.era}/>
		<TextFormGroup id={'rangedWeaponKind'} label={'Kind'} onChange={this.onKindChange} required={true}
		               value={this.props.item.kind}/>
		<TextAreaFormGroup id={"rangedWeaponNote"}
		                   label="Notes"
		                   onChange={this.onNoteChange}
		                   value={this.props.item.notes}/>
	</div>;

}
