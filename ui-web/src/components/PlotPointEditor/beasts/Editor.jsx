import {
	CheckboxFormGroup,
	NumberFormGroup,
	PrependAddon,
	TextAreaFormGroup,
	TextFormGroup
} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import AttributeComponent from '../components/AttributeComponent'
import BaseEditor from '../components/BaseEditor'
import SelectedSkills from './selected_skills/index'
import Index from './special_abilities/index'


export default class Editor extends React.Component {

	static propTypes = {
		id             : PropTypes.string.isRequired,
		skillsAvailable: PropTypes.array.isRequired
	}

	state = {
		selected: ''
	}

	agilityChange            = e => this.props.onChange(Object.assign({}, this.props.item, {agility: e}), this.props.index)
	animalIntelligenceChange = e => this.props.onChange(Object.assign({}, this.props.item, {animalIntelligence: e.target.value}), this.props.index)
	armorChange              = e => this.props.onChange(Object.assign({}, this.props.item, {armor: parseInt(e.target.value, 10)}), this.props.index)
	charismaChange           = e => this.props.onChange(Object.assign({}, this.props.item, {charisma: parseInt(e.target.value, 10)}), this.props.index)
	delete                   = e => {
		e.preventDefault()
		this.props.onDelete(this.props.index)
	}
	descriptionChange        = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index)
	nameChange               = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index)
	paceChange               = e => this.props.onChange(Object.assign({}, this.props.item, {pace: parseInt(e.target.value, 10)}), this.props.index)
	skillListChanged         = skills => {
		this.props.onChange(Object.assign({}, this.props.item, {skills: skills}), this.props.index)
	}
	smartsChange             = e => this.props.onChange(Object.assign({}, this.props.item, {smarts: e}), this.props.index)
	specialAbilitiesChange   = specialAbilities => {this.props.onChange(Object.assign({}, this.props.item, {specialAbilities}), this.props.index)}
	spiritChange             = e => this.props.onChange(Object.assign({}, this.props.item, {spirit: e}), this.props.index)
	strengthChange           = e => this.props.onChange(Object.assign({}, this.props.item, {strength: e}), this.props.index)
	vigorChange              = e => this.props.onChange(Object.assign({}, this.props.item, {vigor: e}), this.props.index)

	render() {
		let unselectedSkills = this.props.skillsAvailable
		unselectedSkills     = unselectedSkills.map((s, i) => ({
			label: `${s.name} (${s.attribute})`,
			value: i.toString(),
		}))
		let component_id     = `BeastEditor-${this.props.index}-${this.props.id}`
		return (
			<BaseEditor id={this.props.id} onDelete={this.delete}>
				<TextFormGroup id={component_id + '-Name'} label='Name' onChange={this.nameChange} required={true}
				               value={this.props.item.name}/>
				<TextAreaFormGroup id={component_id + '-Description'}
				                   label='Description'
				                   onChange={this.descriptionChange}
				                   required={false}
				                   value={this.props.item.description}/>
				<AttributeComponent id={component_id + '-Agility'} onChange={this.agilityChange} value={this.props.item.agility}
				                    prepend={<PrependAddon>Agility</PrependAddon>}/>

				<AttributeComponent id={component_id + '-Smarts'} prepend={<PrependAddon>Smarts</PrependAddon>}
				                    value={this.props.item.smarts}
				                    onChange={this.smartsChange}
				                    append={<div className={'input-group-append'}><CheckboxFormGroup
					                    id={component_id + '-AnimalIntelligence'} label={'Animal Intelligence'}
					                    value={this.props.item.animalIntelligence} onChange={this.animalIntelligenceChange}/>
				                    </div>}/>

				<AttributeComponent id={component_id + '-Spirit'} prepend={<PrependAddon>Spirit</PrependAddon>}
				                    value={this.props.item.spirit}
				                    onChange={this.spiritChange}/>
				<AttributeComponent id={component_id + '-Strength'} prepend={<PrependAddon>Strength</PrependAddon>}
				                    value={this.props.item.strength}
				                    onChange={this.strengthChange}/>
				<AttributeComponent id={component_id + '-Vigor'} prepend={<PrependAddon>Vigor</PrependAddon>}
				                    value={this.props.item.vigor}
				                    onChange={this.vigorChange}/>
				<NumberFormGroup id={component_id + '-Charisma'} label={'Charisma'} value={this.props.item.charisma}
				                 onChange={this.charismaChange}/>
				<NumberFormGroup id={component_id + '-Pace'} label={'Pace'} value={this.props.item.pace}
				                 onChange={this.paceChange}/>
				<NumberFormGroup id={component_id + '-Armor'} label={'Armor'} value={this.props.item.armor || 0}
				                 onChange={this.armorChange}/>
				<SelectedSkills id={component_id} skillsAvailable={unselectedSkills}
				                skills={this.props.item.skills} onChange={this.skillListChanged}/>
				<Index abilities={this.props.item.specialAbilities} id={component_id}
				       onChange={this.specialAbilitiesChange}/>
			</BaseEditor>
		)
	}
}
