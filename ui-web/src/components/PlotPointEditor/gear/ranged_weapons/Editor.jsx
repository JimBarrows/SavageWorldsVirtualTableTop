import {NumberFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import MundaneItemEditor from '../mundane_items/Editor'

export default class Editor extends MundaneItemEditor {

	onDamageChange          = e => this.props.onChange(Object.assign({}, this.props.item, {damage: e.target.value}), this.props.index)
	onLongRangeChange       = e => this.props.onChange(Object.assign({}, this.props.item, {longRange: parseInt(e.target.value, 10)}), this.props.index)
	onMediumRangeChange     = e => this.props.onChange(Object.assign({}, this.props.item, {mediumRange: parseInt(e.target.value, 10)}), this.props.index)
	onMinimumStrengthChange = e => this.props.onChange(Object.assign({}, this.props.item, {minimumStrength: e.target.value}), this.props.index)
	onRateOfFireChange      = e => this.props.onChange(Object.assign({}, this.props.item, {rateOfFire: parseInt(e.target.value, 10)}), this.props.index)
	onShortRangeChange      = e => this.props.onChange(Object.assign({}, this.props.item, {shortRange: parseInt(e.target.value, 10)}), this.props.index)
	onShotsChange           = e => this.props.onChange(Object.assign({}, this.props.item, {shots: parseInt(e.target.value, 10)}), this.props.index)

	additionalFields = () => <div id={`RangedWeaponEditor-${this.props.id}`}>
		<NumberFormGroup id={`RangedWeaponEditor-${this.props.id}-ShortRange`} label={'Short Range'}
		                 onChange={this.onShortRangeChange}
		                 required={true} value={this.props.item.shortRange}/>
		<NumberFormGroup id={`RangedWeaponEditor-${this.props.id}-MediumRange`} label={'Medium Range'}
		                 onChange={this.onMediumRangeChange}
		                 required={true} value={this.props.item.mediumRange}/>
		<NumberFormGroup id={`RangedWeaponEditor-${this.props.id}-LongRange`} label={'Long Range'}
		                 onChange={this.onLongRangeChange}
		                 required={true} value={this.props.item.longRange}/>
		<TextFormGroup id={`RangedWeaponEditor-${this.props.id}-Damage`} label={'Damage'} onChange={this.onDamageChange}
		               required={true}
		               value={this.props.item.damage}/>
		<NumberFormGroup id={`RangedWeaponEditor-${this.props.id}-RateOfFire`} label={'Rate of Fire'}
		                 onChange={this.onRateOfFireChange}
		                 required={true} value={this.props.item.rateOfFire}/>
		<NumberFormGroup id={`RangedWeaponEditor-${this.props.id}-Shots`} label={'Shots'} onChange={this.onShotsChange}
		                 required={true} value={this.props.item.shots}/>
		<TextFormGroup id={`RangedWeaponEditor-${this.props.id}-MinimumStrength`} label={'Minimum Strength'}
		               onChange={this.onMinimumStrengthChange}
		               required={true} value={this.props.item.minimumStrength}/>
		{this.additionalRangedWeaponFields ? this.additionalRangedWeaponFields() : ''}
	</div>

}
