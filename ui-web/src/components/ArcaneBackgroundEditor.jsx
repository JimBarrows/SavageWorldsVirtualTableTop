import React from 'react';
import AttributeFormGroup from './AttributeFormGroup';
import BaseEditor from './BaseEditor';
import NumberFormGroup from './NumberFormGroup';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class ArcaneBackgroundEditor extends React.Component {

	attributeChange   = e => this.props.onChange(Object.assign({}, this.props.item, {arcaneAbility: e.target.value}), this.props.index);
	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	onDelete          = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	skillChange       = e => this.props.onChange(Object.assign({}, this.props.item, {skill: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='arcaneBackgroundName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<TextAreaFormGroup id={"arcaneBackgroundDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
					<TextFormGroup id='arcaneBackgroundSkill' label='Skill' onChange={this.skillChange} required={true}
					               value={this.props.item.skill}/>
					<AttributeFormGroup id={'arcaneBackgroundSkillAttribute'}
					                    onChange={this.attributeChange}
					                    attribute={this.props.item.attribute}/>
					<NumberFormGroup id={'arcaneBackgroundStartingPowers'} label='Starting Powers' onChange={this.costChange}
					                 required={true}
					                 value={this.props.item.cost}/>

				</BaseEditor>
		);
	}
}
