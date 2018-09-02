import {NumberFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import RangedWeaponEditor from './RangedWeaponEditor'

export default class SpecialWeaponsEditor extends RangedWeaponEditor {

	onApChange            = e => this.props.onChange(Object.assign({}, this.props.item, {armorPiercing: parseInt(e.target.value, 10)}), this.props.index)
	onBurstTemplateChange = e => this.props.onChange(Object.assign({}, this.props.item, {burstTemplate: e.target.value}), this.props.index)

	additionalRangedWeaponFields = () => <div>
		<NumberFormGroup id={`SpecialWeaponsEditor-${this.props.id}-ArmorPiercing`}
		                 label={'Armor Piercing'}
		                 onChange={this.onApChange}
		                 required={true}
		                 value={this.props.item.armorPiercing}/>
		<TextFormGroup id={`SpecialWeaponsEditor-${this.props.id}-BurstTemplate`} label={'Burst Template'}
		               onChange={this.onBurstTemplateChange}
		               value={this.props.item.burstTemplate}/>
	</div>

}
