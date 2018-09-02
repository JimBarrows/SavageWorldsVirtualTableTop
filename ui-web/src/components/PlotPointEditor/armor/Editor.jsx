import {NumberFormGroup} from 'bootstrap-react-components'
import React from 'react'
import MundaneItemEditor from '../gear/mundane_items/Editor'

export default class Editor extends MundaneItemEditor {

	onArmorChange = e => this.props.onChange(Object.assign({}, this.props.item, {armor: parseInt(e.target.value, 10)}), this.props.index)


	additionalFields = () => <div id={'ArmorEditorComponent-' + this.props.id}>
		<NumberFormGroup id={'armorArmor'} label={'Armor'} onChange={this.onArmorChange} required={true}
		                 value={this.props.item.armor}/>


	</div>;

};


