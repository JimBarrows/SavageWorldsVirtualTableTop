import React from 'react';
import PropTypes from 'prop-types';
import MundaneItemEditor from './MundaneItemEditor';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class HandWeaponEditor extends MundaneItemEditor {

	static propTypes = {
		id      : PropTypes.string.isRequired,
		item    : PropTypes.object.isRequired,
		onChange: PropTypes.func.isRequired
	};

	onDamageChange = e => this.props.onChange(Object.assign({}, this.props.item, {damage: e.target.value}), this.props.index);
	onEraChange    = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
	onKindChange   = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
	onNoteChange   = e => this.props.onChange(Object.assign({}, this.props.item, {notes: e.target.value}), this.props.index);


	additionalFields = () => <div>
		<TextFormGroup id={'handWeaponDamage'} label={'Damage'} onChange={this.onDamageChange} required={true}
		               value={this.props.item.damage}/>
		<TextFormGroup id={'handWeaponEra'} label={'Era'} onChange={this.onEraChange} required={true}
		               value={this.props.item.era}/>
		<TextFormGroup id={'handWeaponKind'} label={'Kind'} onChange={this.onKindChange} required={true}
		               value={this.props.item.kind}/>
		<TextAreaFormGroup id={"handWeaponNote"}
		                   label="Notes"
		                   onChange={this.onNoteChange}
		                   value={this.props.item.notes}/>
	</div>;

}

