import React from 'react';
import PropTypes from 'prop-types';
import NumberFormGroup from './NumberFormGroup';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class ArmorEditor extends React.Component {

	static propTypes = {
		id      : PropTypes.string.isRequired,
		item    : PropTypes.object.isRequired,
		onChange: PropTypes.func.isRequired
	};

	onArmorChange = e => this.props.onChange(Object.assign({}, this.props.item, {armor: e.target.value}), this.props.index);
	onEraChange   = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
	onKindChange  = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
	onNoteChange  = e => this.props.onChange(Object.assign({}, this.props.item, {notes: e.target.value}), this.props.index);

	render() {
		return <div id={'ArmorEditorComponent_' + this.props.id}>
			<NumberFormGroup id={'armorArmor'} label={'Armor'} onChange={this.onArmorChange} required={true}
			               value={this.props.item.damage}/>
			<TextFormGroup id={'armorEra'} label={'Era'} onChange={this.onEraChange} required={true}
			               value={this.props.item.era}/>
			<TextFormGroup id={'armorKind'} label={'Kind'} onChange={this.onKindChange} required={true}
			               value={this.props.item.kind}/>
			<TextAreaFormGroup id={"armorNote"}
			                   label="Notes"
			                   onChange={this.onNoteChange}
			                   value={this.props.item.notes}/>
		</div>;
	}
};


