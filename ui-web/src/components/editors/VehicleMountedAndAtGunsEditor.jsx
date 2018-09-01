import {NumberFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import MundaneItemEditor from './MundaneItemEditor'

export default class VehicleMountedAndAtGunsEditor extends MundaneItemEditor {

	longRangeChange       = e => this.props.onChange(Object.assign({}, this.props.item, {longRange: parseInt(e.target.value, 10)}), this.props.index)
	mediumRangeChange     = e => this.props.onChange(Object.assign({}, this.props.item, {mediumRange: parseInt(e.target.value, 10)}), this.props.index)
	rateOfFireChange      = e => this.props.onChange(Object.assign({}, this.props.item, {rateOfFire: parseInt(e.target.value, 10)}), this.props.index)
	shortRangeChange      = e => this.props.onChange(Object.assign({}, this.props.item, {shortRange: parseInt(e.target.value, 10)}), this.props.index)
	apDamageChange        = e => this.props.onChange(Object.assign({}, this.props.item, {apDamage: e.target.value}), this.props.index)
	apArmorPiercingChange = e => this.props.onChange(Object.assign({}, this.props.item, {apArmorPiercing: parseInt(e.target.value, 10)}), this.props.index)
	heDamageChange        = e => this.props.onChange(Object.assign({}, this.props.item, {heDamage: e.target.value}), this.props.index)
	heArmorPiercingChange = e => this.props.onChange(Object.assign({}, this.props.item, {heArmorPiercing: parseInt(e.target.value, 10)}), this.props.index)
	heBurstTemplateChange = e => this.props.onChange(Object.assign({}, this.props.item, {heBurstTemplate: e.target.value}), this.props.index)

	additionalFields = () => <div>
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

	</div>
}
