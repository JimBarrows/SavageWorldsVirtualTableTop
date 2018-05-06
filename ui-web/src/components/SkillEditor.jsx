import {SelectFormGroup} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';
import BaseEditor from './BaseEditor';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

class SkillEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps = {
		id: 'SkillEditor'
	};

	attributeChange = e => this.props.onChange(Object.assign({}, this.props.item, {attribute: e.target.value}), this.props.index);

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);

	nameChange = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);

	onDelete = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};

	render() {
		return (

				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='skillName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<SelectFormGroup id={"skillAttribute"} label={'Attribute'} onChange={this.attributeChange}
					                 options={[{label: 'Agility', value: 'Agility'}, {
						                 label: 'Smarts',
						                 value: 'Smarts'
					                 }, {label: 'Spirit', value: 'Spirit'}, {
						                 label: 'Strength',
						                 value: 'Strength'
					                 }, {label: 'Vigor'}]}
					                 value={this.props.item.attribute}/>
					<TextAreaFormGroup id={'skillDescription'} label={'Description'} onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
				</BaseEditor>
		);
	}
}

export default SkillEditor;