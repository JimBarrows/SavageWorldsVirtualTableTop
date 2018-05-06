import PropTypes from 'prop-types';
import React from 'react';
import BaseEditor from './BaseEditor';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class EdgeEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps       = {
		id: 'EdgeEditor'
	};
	       categoryChange     = e => this.props.onChange(Object.assign({}, this.props.edge, {category: e.target.value}), this.props.index);
	       descriptionChange  = e => this.props.onChange(Object.assign({}, this.props.edge, {description: e.target.value}), this.props.index);
	       effectsChange      = e => this.props.onChange(Object.assign({}, this.props.edge, {effects: e.target.value}), this.props.index);
	       nameChange         = e => this.props.onChange(Object.assign({}, this.props.edge, {name: e.target.value}), this.props.index);
	       onDelete           = event => {
		       event.preventDefault();
		       this.props.onDelete(this.props.index);
	       };
	       requirementsChange = e => this.props.onChange(Object.assign({}, this.props.edge, {requirements: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='edgeName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.edge.name}/>
					<TextFormGroup id='edgeCategory' label='Category' onChange={this.categoryChange} required={true}
					               value={this.props.edge.category}/>
					<TextFormGroup id='edgeRequirements' label='Requirements' onChange={this.requirementsChange} required={true}
					               value={this.props.edge.requirements}/>
					<TextAreaFormGroup id={'edgeDescription'} label={'Description'} onChange={this.descriptionChange}
					                   value={this.props.edge.description}/>
					<TextFormGroup id='edgeEffects' label='Effects' onChange={this.effectsChange} required={true}
					               value={this.props.edge.effects}/>
				</BaseEditor>
		);
	}
}

