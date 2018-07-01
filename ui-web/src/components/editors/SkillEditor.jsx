import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import AttributeSelectFormGroup from '../formgroups/AttributeSelectFormGroup'
import BaseEditor from './BaseEditor'

class SkillEditor extends React.Component {

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
					<AttributeSelectFormGroup id={'skillAttribute'}
					                          onChange={this.attributeChange}
					                          attribute={this.props.item.attribute}/>
					<TextAreaFormGroup id={'skillDescription'} label={'Description'} onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
				</BaseEditor>
		);
	}
}

export default SkillEditor;