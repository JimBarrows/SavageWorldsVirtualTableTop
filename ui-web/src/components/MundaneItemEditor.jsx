import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import BaseEditor from './BaseEditor'

export default class MundaneItemEditor extends React.Component {

	costChange        = e => this.props.onChange(Object.assign({}, this.props.item, {cost: parseInt(e.target.value, 10)}), this.props.index);
	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	onDelete          = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	weightChange      = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='mundaneItemName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<TextAreaFormGroup id={"mundaneItemDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
					<NumberFormGroup id={'mundaneItemCost'} label='Cost' onChange={this.costChange} required={true}
					                 value={this.props.item.cost}/>
					<NumberFormGroup id={'mundaneItemWeight'} label='Weight' onChange={this.weightChange} required={true}
					                 value={this.props.item.weight}/>
					{this.additionalFields ? this.additionalFields() : ''}
				</BaseEditor>
		);
	}

	static defaultProps = {
		id: 'MundaneItemEditorComponent'
	};
}

