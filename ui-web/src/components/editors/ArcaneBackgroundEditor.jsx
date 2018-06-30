import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import AttributeFormGroup from '../formgroups/AttributeFormGroup'
import BaseEditor from './BaseEditor'

export default class ArcaneBackgroundEditor extends React.Component {

	attributeChange   = e => this.props.onChange(Object.assign({}, this.props.item, {attribute: e.target.value}), this.props.index);
	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	onDelete          = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	skillNameChange       = e => this.props.onChange(Object.assign({}, this.props.item, {skillName: e.target.value}), this.props.index);
	startingPowersChange       = e => this.props.onChange(Object.assign({}, this.props.item, {startingPowers: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='arcaneBackgroundName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<TextAreaFormGroup id={"arcaneBackgroundDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
					<TextFormGroup id='arcaneBackgroundSkill' label='Skill' onChange={this.skillNameChange} required={true}
					               value={this.props.item.skillName}/>
					<AttributeFormGroup id={'arcaneBackgroundSkillAttribute'}
					                    onChange={this.attributeChange}
					                    attribute={this.props.item.attribute}/>
					<NumberFormGroup id={'arcaneBackgroundStartingPowers'} label='Starting Powers' onChange={this.startingPowersChange}
					                 required={true}
					                 value={this.props.item.startingPowers}/>

				</BaseEditor>
		);
	}
}
