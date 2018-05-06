import PropTypes from 'prop-types';
import React from 'react';
import BaseEditor from './BaseEditor';
import NumberFormGroup from './NumberFormGroup';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class MundaneItemEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps      = {
		id: 'MundaneItemEditorComponent'
	};
	       costChange        = e => this.props.onChange(Object.assign({}, this.props.mundaneItem, {cost: parseInt(e.target.value, 10)}), this.props.index);
	       descriptionChange = e => this.props.onChange(Object.assign({}, this.props.mundaneItem, {description: e.target.value}), this.props.index);
	       nameChange        = e => this.props.onChange(Object.assign({}, this.props.mundaneItem, {name: e.target.value}), this.props.index);
	       onDelete          = event => {
		       event.preventDefault();
		       this.props.onDelete(this.props.index);
	       };
	       weightChange      = e => this.props.onChange(Object.assign({}, this.props.mundaneItem, {name: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='mundaneItemName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.mundaneItem.name}/>
					<TextAreaFormGroup id={"mundaneItemDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={this.props.mundaneItem.description}/>
					<NumberFormGroup id={'mundaneItemCost'} label='Cost' onChange={this.costChange} required={true}
					                 value={this.props.mundaneItem.cost}/>
					<NumberFormGroup id={'mundaneItemWeight'} label='Weight' onChange={this.weightChange} required={true}
					                 value={this.props.mundaneItem.weight}/>
				</BaseEditor>
		);
	}
}

