import TextAreaFormGroup  from 'bootstrap-react-components/distribution/formgroups/TextAreaFormGroup'
import TextFormGroup      from 'bootstrap-react-components/distribution/formgroups/TextFormGroup'
import PropTypes          from 'prop-types'
import React              from 'react'
import AttributeFormGroup from './AttributeFormGroup'
import BaseEditor         from './BaseEditor'
import SelectedSkillList  from './SelectedSkillList'

export default class BeastEditor extends React.Component {

	static propTypes = {
		id             : PropTypes.string.isRequired,
		skillsAvailable: PropTypes.array.isRequired
	};

	static defaultProps = {
		id: "BeastEditor"
	};

	state = {
		selected: ''
	};

	addSkill          = skill => this.props.onChange(Object.assign({}, this.props.item, {skills: [skill, ...this.props.item.skills]}));
	agilityChange     = e => this.props.onChange(Object.assign({}, this.props.item, {agility: e}), this.props.index);
	delete            = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	skillListChanged  = (idx, skill) => this.props.onChange(Object.assign({}, this.props.item, {skills: this.props.skills.map((s, i) => i === idx ? skill : s)}))
	smartsChange      = e => this.props.onChange(Object.assign({}, this.props.item, {smarts: e}), this.props.index);
	spiritChange      = e => this.props.onChange(Object.assign({}, this.props.item, {spirit: e}), this.props.index);
	strengthChange    = e => this.props.onChange(Object.assign({}, this.props.item, {strength: e}), this.props.index);
	vigorChange       = e => this.props.onChange(Object.assign({}, this.props.item, {vigor: e}), this.props.index);

	render() {
		let chosenSkillNames = this.props.item.skills.map(s => s.skill.name);
		let unselectedSkills = this.props.skillsAvailable.filter(s => !chosenSkillNames.includes(s)).map((s, i) => ({
			value: i,
			label: `${s.name} (${s.attribute})`
		}));
		return (
				<BaseEditor id={this.props.id} onDelete={this.delete}>
					<TextFormGroup id='beastName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<TextAreaFormGroup id={'beastDescription'}
					                   label='Description'
					                   onChange={this.descriptionChange}
					                   required={false}
					                   value={this.props.item.description}/>
					<AttributeFormGroup id={'agility'} label='Agility' value={this.props.item.agility}
					                    onChange={this.agilityChange}/>
					<AttributeFormGroup id={'smarts'} label='Smarts' value={this.props.item.smarts}
					                    onChange={this.smartsChange}/>
					<AttributeFormGroup id={'spirit'} label='Spirit' value={this.props.item.spirit}
					                    onChange={this.spiritChange}/>
					<AttributeFormGroup id={'strength'} label='Strength' value={this.props.item.strength}
					                    onChange={this.strengthChange}/>
					<AttributeFormGroup id={'vigor'} label='Vigor' value={this.props.item.vigor}
					                    onChange={this.vigorChange}/>
					<SelectedSkillList id={'beastSkills'} skillsAvailable={unselectedSkills}
					                   selectedSkills={this.props.item.skills} onChange={this.addSkill}/>
				</BaseEditor>
		);
	}
}

