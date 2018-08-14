import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import MundaneItemEditor from './MundaneItemEditor'

export default class ArmorEditor extends MundaneItemEditor {

	onArmorChange = e => this.props.onChange(Object.assign({}, this.props.item, {armor: parseInt(e.target.value, 10)}), this.props.index)
	onEraChange   = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
	onKindChange  = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
	onNoteChange  = e => this.props.onChange(Object.assign({}, this.props.item, {note: e.target.value}), this.props.index)

	additionalFields = () => <div id={'ArmorEditorComponent-' + this.props.id}>
		<NumberFormGroup id={'armorArmor'} label={'Armor'} onChange={this.onArmorChange} required={true}
		                 value={this.props.item.armor}/>
		<TextFormGroup id={'armorEra'} label={'Era'} onChange={this.onEraChange} required={true}
		               value={this.props.item.era}/>
		<TextFormGroup id={'armorKind'} label={'Kind'} onChange={this.onKindChange} required={true}
		               value={this.props.item.kind}/>
		<TextAreaFormGroup id={"armorNote"}
		                   label="Note"
		                   onChange={this.onNoteChange}
		                   value={this.props.item.note}/>
	</div>;

};


