import {Panel, PanelBody, PanelHeader, RemoveButton, SelectFormGroup} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

class SkillEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps = {
		id: 'SkillEditor'
	};

	attributeChange = e => this.props.onChange({
		attribute  : e.target.value,
		description: this.props.skill.description,
		name       : this.props.skill.name,
	}, this.props.index);

	descriptionChange = e => this.props.onChange({
		attribute  : this.props.skill.attribute,
		description: e.target.value,
		name       : this.props.skill.name,
	}, this.props.index);

	nameChange = e => this.props.onChange({
		attribute  : this.props.skill.attribute,
		description: this.props.skill.description,
		name       : e.target.value,
	}, this.props.index);

	onDelete = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};

	render() {
		return (

				<Panel id={this.props.id}>
					<PanelHeader id={this.props.id}>
						<div className={'btn-group pull-right'}>
							<RemoveButton id={this.props.id} onClick={this.onDelete}/>
						</div>
					</PanelHeader>
					<PanelBody id={this.props.id}>
						<TextFormGroup id='skillName' label='Name' onChange={this.nameChange} required={true}
						               value={this.props.skill.name}/>
						<SelectFormGroup id={"skillAttribute"} label={'Attribute'} onChange={this.attributeChange}
						                 options={[{label: 'Agility', value: 'Agility'}, {
							                 label: 'Smarts',
							                 value: 'Smarts'
						                 }, {label: 'Spirit', value: 'Spirit'}, {
							                 label: 'Strength',
							                 value: 'Strength'
						                 }, {label: 'Vigor'}]}
						                 value={this.props.skill.attribute}/>
						<TextAreaFormGroup id={'skillDescription'} label={'Description'} onChange={this.descriptionChange}
						                   value={this.props.skill.description}/>
					</PanelBody>
				</Panel>
		);
	}
}

export default SkillEditor;