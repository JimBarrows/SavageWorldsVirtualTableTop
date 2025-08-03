import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import BaseEditor from '../../../BaseEditor'

export default class ModifierEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired,
		index: PropTypes.number.isRequired,
		item: PropTypes.shape({
			name: PropTypes.string,
			description: PropTypes.string,
			powerPointModifier: PropTypes.number
		}).isRequired,
		onChange: PropTypes.func.isRequired,
		onDelete: PropTypes.func.isRequired
	}

	nameChange = e => {
		this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	};
	
	descriptionChange = e => {
		this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	};
	
	powerPointModifierChange = e => {
		const value = parseInt(e.target.value, 10) || 0;
		this.props.onChange(Object.assign({}, this.props.item, {powerPointModifier: value}), this.props.index);
	};
	
	onDelete = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};

	render() {
		const {item} = this.props;
		const idPrefix = `${this.props.id}-${this.props.index}`;
		
		return (
			<BaseEditor id={this.props.id} onDelete={this.onDelete}>
				<TextFormGroup 
					id={`FormControl-text-TextFormGroup-ModifierEditor-${idPrefix}-Name`}
					label='Name' 
					onChange={this.nameChange} 
					required={true}
					value={item.name}
				/>
				<TextAreaFormGroup 
					id={`TextAreaFormGroup-ModifierEditor-${idPrefix}-Description`}
					label="Description"
					onChange={this.descriptionChange}
					required={true}
					value={item.description}
				/>
				<NumberFormGroup 
					id={`FormControl-number-NumberFormGroup-ModifierEditor-${idPrefix}-PowerPointModifier`}
					label={'Power Point Modifier'} 
					onChange={this.powerPointModifierChange}
					required={true} 
					value={item.powerPointModifier}
				/>
			</BaseEditor>
		);
	}
}