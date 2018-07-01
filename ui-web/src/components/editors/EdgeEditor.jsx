import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import BaseEditor from './BaseEditor'

export default class EdgeEditor extends React.Component {

	static defaultProps       = {
		id: 'EdgeEditor'
	};
	       categoryChange     = e => this.props.onChange(Object.assign({}, this.props.item, {category: e.target.value}), this.props.index);
	       descriptionChange  = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	       effectsChange      = e => this.props.onChange(Object.assign({}, this.props.item, {effects: e.target.value}), this.props.index);
	       nameChange         = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	       onDelete           = event => {
		       event.preventDefault();
		       this.props.onDelete(this.props.index);
	       };
	       requirementsChange = e => this.props.onChange(Object.assign({}, this.props.item, {requirements: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='edgeName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<TextFormGroup id='edgeCategory' label='Category' onChange={this.categoryChange} required={true}
					               value={this.props.item.category}/>
					<TextFormGroup id='edgeRequirements' label='Requirements' onChange={this.requirementsChange} required={true}
					               value={this.props.item.requirements}/>
					<TextAreaFormGroup id={'edgeDescription'} label={'Description'} onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
					<TextFormGroup id='edgeEffects' label='Effects' onChange={this.effectsChange} required={true}
					               value={this.props.item.effects}/>
				</BaseEditor>
		);
	}
}

