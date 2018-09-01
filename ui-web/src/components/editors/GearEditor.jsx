import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import BaseEditor from './BaseEditor'

export default class GearEditor extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id: PropTypes.string.isRequired
	}

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index)
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index)
	onKindChange      = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index)
	onNoteChange      = e => this.props.onChange(Object.assign({}, this.props.item, {note: e.target.value}), this.props.index)
	onEraChange       = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index)
	onDelete          = event => {
		event.preventDefault()
		this.props.onDelete(this.props.index)
	}

	render() {
		let {id}        = this.props
		let componentId = `GearEditor-${id}`
		return (
			<BaseEditor id={componentId} onDelete={this.onDelete}>
				<TextFormGroup id={componentId + '-Name'} label='Name' onChange={this.nameChange} required={true}
				               value={this.props.item.name}/>
				<TextAreaFormGroup id={componentId + "-Description"}
				                   label="Description"
				                   onChange={this.descriptionChange}
				                   value={this.props.item.description}/>
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
		)
	}
}

