import {NumberFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React                            from 'react'
import MundaneItemEditor                from '../mundane_items/Editor'

export default class Editor extends MundaneItemEditor {

	apDamageChange        = e => this.props.onChange(Object.assign({}, this.props.item, {apDamage: e.target.value}), this.props.index)
	apArmorPiercingChange = e => this.props.onChange(Object.assign({}, this.props.item, {apArmorPiercing: parseInt(e.target.value, 10)}), this.props.index)
	heDamageChange        = e => this.props.onChange(Object.assign({}, this.props.item, {heDamage: e.target.value}), this.props.index)
	heArmorPiercingChange = e => this.props.onChange(Object.assign({}, this.props.item, {heArmorPiercing: parseInt(e.target.value, 10)}), this.props.index)
	heBurstTemplateChange = e => this.props.onChange(Object.assign({}, this.props.item, {heBurstTemplate: e.target.value}), this.props.index)

	additionalRangedWeaponFields = () => <div>
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

	</div>
}
