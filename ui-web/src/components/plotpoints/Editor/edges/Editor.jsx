import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React                              from 'react'
import BaseEditor                         from '../components/BaseEditor'

export default class Editor extends React.Component {

	static defaultProps = {
		id: 'Editor'
	}

	categoryChange     = e => this.props.onChange(Object.assign({}, this.props.item, {category: e.target.value}), this.props.index)
	descriptionChange  = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index)
	effectsChange      = e => this.props.onChange(Object.assign({}, this.props.item, {effects: e.target.value}), this.props.index)
	nameChange         = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index)
	onDelete           = event => {
		event.preventDefault()
		this.props.onDelete(this.props.index)
	}
	requirementsChange = e => this.props.onChange(Object.assign({}, this.props.item, {requirements: e.target.value}), this.props.index)

	render() {
		let {id, item}  = this.props
		let componentId = `EdgeEditor-${id}`
		return (
			<BaseEditor id={componentId} onDelete={this.onDelete}>
				<TextFormGroup id={`${componentId}-Name`} label='Name' onChange={this.nameChange} required={true}
				               value={item.name}/>
				<TextFormGroup id={`${componentId}-Category`} label='Category' onChange={this.categoryChange} required={true}
				               value={item.category}/>
				<TextFormGroup id={`${componentId}-Requirements`} label='Requirements' onChange={this.requirementsChange}
				               required={true} value={item.requirements}/>
				<TextAreaFormGroup id={`${componentId}-Description`} label={'Description'} onChange={this.descriptionChange}
				                   value={item.description}/>
				<TextFormGroup id={`${componentId}-Effects`} label='Effects' onChange={this.effectsChange} required={true}
				               value={item.effects}/>
			</BaseEditor>
		)
	}
}

