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
	weightChange      = e => this.props.onChange(Object.assign({}, this.props.item, {weight: parseInt(e.target.value, 10)}), this.props.index)
	onKindChange      = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index)
	onNoteChange      = e => this.props.onChange(Object.assign({}, this.props.item, {note: e.target.value}), this.props.index)
	onEraChange       = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index)

	render() {
		let componentId = `MundaneItem-${this.props.id}`
		return (
			<BaseEditor id={componentId} onDelete={this.onDelete}>
				<TextFormGroup id={componentId + '-Name'} label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
				<TextAreaFormGroup id={componentId + "-Description"}
				                   label="Description"
				                   onChange={this.descriptionChange}
				                   value={this.props.item.description}/>
				<NumberFormGroup id={componentId + '-Cost'} label='Cost' onChange={this.costChange} required={true}
				                 value={this.props.item.cost}/>
				<NumberFormGroup id={componentId + '-Weight'} label='Weight' onChange={this.weightChange} required={true}
				                 value={this.props.item.weight}/>
					{this.additionalFields ? this.additionalFields() : ''}
				<TextFormGroup id={componentId + '-Era'} label={'Era'} onChange={this.onEraChange} required={true}
				               value={this.props.item.era}/>
				<TextFormGroup id={componentId + '-Kind'} label={'Kind'} onChange={this.onKindChange} required={true}
				               value={this.props.item.kind}/>
				<TextAreaFormGroup id={componentId + '-Note'}
				                   label="Note"
				                   onChange={this.onNoteChange}
				                   value={this.props.item.note}/>
				</BaseEditor>
		);
	}

	static defaultProps = {
		id: 'MundaneItemEditorComponent'
	};
}

