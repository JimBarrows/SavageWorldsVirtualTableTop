import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import BaseEditor from './BaseEditor'

export default class AmmunitionEditor extends React.Component {

	costChange        = e => this.props.onChange(Object.assign({}, this.props.item, {cost: e.target.value}), this.props.index);
	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	noteChange        = e => this.props.onChange(Object.assign({}, this.props.item, {notes: e.target.value}), this.props.index);
	delete            = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	weightChange      = e => this.props.onChange(Object.assign({}, this.props.item, {weight: parseInt(e.target.value, 10)}), this.props.index);

	render() {
		return <BaseEditor id={this.props.id} onDelete={this.delete}>
			<TextFormGroup id='ammunitionName' label='Name' onChange={this.nameChange} required={true}
			               value={this.props.item.name}/>
			<TextFormGroup id='ammunitionCost' label='Cost' onChange={this.costChange} required={true}
			               value={this.props.item.cost}/>
			<TextFormGroup id={'ammunitionWeight'} label='Weight' onChange={this.weightChange} required={true}
			               value={this.props.item.weight}/>
			<TextAreaFormGroup id={'ammunitionNote'}
			                   label='Notes'
			                   onChange={this.noteChange}
			                   required={true}
			                   value={this.props.item.notes}/>
		</BaseEditor>;
	}
}
