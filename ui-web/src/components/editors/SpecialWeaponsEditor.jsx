import {NumberFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import MundaneItemEditor from './MundaneItemEditor'

export default class SpecialWeaponsEditor extends MundaneItemEditor {

	onApChange              = e => this.props.onChange(Object.assign({}, this.props.item, {armorPiercing: parseInt(e.target.value, 10)}), this.props.index)
	onDamageChange          = e => this.props.onChange(Object.assign({}, this.props.item, {damage: e.target.value}), this.props.index)
	onLongRangeChange       = e => this.props.onChange(Object.assign({}, this.props.item, {longRange: parseInt(e.target.value, 10)}), this.props.index)
	onMediumRangeChange     = e => this.props.onChange(Object.assign({}, this.props.item, {mediumRange: parseInt(e.target.value, 10)}), this.props.index)
	onMinimumStrengthChange = e => this.props.onChange(Object.assign({}, this.props.item, {minimumStrength: e.target.value}), this.props.index)
	onRateOfFireChange      = e => this.props.onChange(Object.assign({}, this.props.item, {rateOfFire: parseInt(e.target.value, 10)}), this.props.index)
	onShortRangeChange      = e => this.props.onChange(Object.assign({}, this.props.item, {shortRange: parseInt(e.target.value, 10)}), this.props.index)
	onBurstTemplateChange   = e => this.props.onChange(Object.assign({}, this.props.item, {burstTemplate: e.target.value}), this.props.index)

	additionalFields = () => <div>
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
		               value={this.props.item.burstTemplate}/>
	</div>

}
