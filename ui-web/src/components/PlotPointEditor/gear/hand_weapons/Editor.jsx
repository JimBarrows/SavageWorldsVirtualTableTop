import {TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import MundaneItemEditor from '../mundane_items/Editor'

export default class Editor extends MundaneItemEditor {

	onDamageChange = e => this.props.onChange(Object.assign({}, this.props.item, {damage: e.target.value}), this.props.index);


	additionalFields = () => <div>
		<TextFormGroup id={'handWeaponDamage'} label={'Damage'} onChange={this.onDamageChange} required={true}
		               value={this.props.item.damage}/>
	</div>;

}

