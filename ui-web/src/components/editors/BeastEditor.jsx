import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import AttributeFormGroup from '../formgroups/AttributeFormGroup'
import SelectedSkillList from '../lists/SelectedSkillList'
import SpecialAbilitiesList from '../lists/SpecialAbilitiesList'
import BaseEditor from './BaseEditor'


export default class BeastEditor extends React.Component {

  static propTypes = {
    id             : PropTypes.string.isRequired,
    skillsAvailable: PropTypes.array.isRequired
  }

  state = {
    selected: ''
  }

	agilityChange     = e => this.props.onChange(Object.assign({}, this.props.item, {agility: e}), this.props.index)
	abilitiesChange   = (abilities) => {
		this.props.onChange(Object.assign({}, this.props.item, {
			abilities
		}), this.props.index)
	}
	delete            = event => {
    event.preventDefault()
    this.props.onDelete(this.props.index)
  }
	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index)
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index)
	skillListChanged  = skills => {
    this.props.onChange(Object.assign({}, this.props.item, {skills: skills}), this.props.index)
  }
	smartsChange      = e => this.props.onChange(Object.assign({}, this.props.item, {smarts: e}), this.props.index)
  spiritChange      = e => this.props.onChange(Object.assign({}, this.props.item, {spirit: e}), this.props.index)
  strengthChange    = e => this.props.onChange(Object.assign({}, this.props.item, {strength: e}), this.props.index)
  vigorChange       = e => this.props.onChange(Object.assign({}, this.props.item, {vigor: e}), this.props.index)

  render() {
    let unselectedSkills = this.props.skillsAvailable//.filter(s => chosenSkillNames.includes(s.name))
    unselectedSkills     = unselectedSkills.map((s, i) => ({
      label: `${s.name} (${s.attribute})`,
      value: i.toString(),
    }))
	  let component_id     = `BeastEditor-${this.props.id}`
    return (
      <BaseEditor id={this.props.id} onDelete={this.delete}>
	      <TextFormGroup id={component_id + '-Name'} label='Name' onChange={this.nameChange} required={true}
	                     value={this.props.item.name}/>
	      <TextAreaFormGroup id={component_id + '-Description'}
	                         label='Description'
	                         onChange={this.descriptionChange}
	                         required={false}
	                         value={this.props.item.description}/>
	      <AttributeFormGroup id={component_id + '-Agility'} label='Agility' value={this.props.item.agility}
	                          onChange={this.agilityChange}/>
	      <AttributeFormGroup id={component_id + '-Smarts'} label='Smarts' value={this.props.item.smarts}
	                          onChange={this.smartsChange}/>
	      <AttributeFormGroup id={component_id + '-Spirit'} label='Spirit' value={this.props.item.spirit}
	                          onChange={this.spiritChange}/>
	      <AttributeFormGroup id={component_id + '-Strength'} label='Strength' value={this.props.item.strength}
	                          onChange={this.strengthChange}/>
	      <AttributeFormGroup id={component_id + '-Vigor'} label='Vigor' value={this.props.item.vigor}
	                          onChange={this.vigorChange}/>
	      <SelectedSkillList id={component_id + '-Skills'} skillsAvailable={unselectedSkills}
	                         skills={this.props.item.skills} onChange={this.skillListChanged}/>
	      <SpecialAbilitiesList abilities={this.props.item.abilities} id={component_id + '-SpecialAbilities'}
	                            onChange={this.abilitiesChange}/>
      </BaseEditor>
    )
  }
}
