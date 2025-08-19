import {
	CheckboxFormGroup,
	NumberFormGroup,
	PrependAddon,
	TextAreaFormGroup,
	TextFormGroup
}                         from 'bootstrap-react-components'
import PropTypes          from 'prop-types'
import React              from 'react'
import AttributeComponent from '../AttributeComponent'
import BaseEditor         from '../BaseEditor'
import SelectedEdges      from './selected_edges/index'
import SelectedHindrances from './selected_hindrances/index'
import SelectedSkills     from './selected_skills/index'

export default class CharacterSheet extends React.Component {

	static propTypes = {
		edgesAvailable     : PropTypes.array.isRequired,
		hindrancesAvailable: PropTypes.array.isRequired,
		id                 : PropTypes.string.isRequired,
		item               : PropTypes.object.isRequired,
		skillsAvailable    : PropTypes.array.isRequired,
		onChange           : PropTypes.func.isRequired,
		onDelete           : PropTypes.func.isRequired,
		index              : PropTypes.number.isRequired,
		children           : PropTypes.node
	}

	state = {
		selected: ''
	}

	updateCharacterProperty = (property, value) => {
		this.props.onChange({
			...this.props.item,
			[property]: value
		});
	}

	agilityChange            = e => this.updateCharacterProperty('agility', e)
	animalIntelligenceChange = e => this.updateCharacterProperty('animalIntelligence', e.target.value)
	charismaChange           = e => this.updateCharacterProperty('charisma', parseInt(e.target.value, 10))
	delete                   = e => {
		e.preventDefault()
		this.props.onDelete(this.props.index)
	}
	descriptionChange        = e => this.updateCharacterProperty('description', e.target.value)
	edgeListChanged          = edges => this.updateCharacterProperty('edges', edges)
	hindranceListChanged     = hindrances => this.updateCharacterProperty('hindrances', hindrances)
	nameChange               = e => this.updateCharacterProperty('name', e.target.value)
	paceChange               = e => this.updateCharacterProperty('pace', parseInt(e.target.value, 10))
	skillListChanged         = skills => this.updateCharacterProperty('skills', skills)
	smartsChange             = e => this.updateCharacterProperty('smarts', e)
	spiritChange             = e => this.updateCharacterProperty('spirit', e)
	strengthChange           = e => this.updateCharacterProperty('strength', e)
	vigorChange              = e => this.updateCharacterProperty('vigor', e)

	render () {
		let {id}         = this.props
		let component_id = `CharacterSheet-${id}`
		return (
			<BaseEditor id={this.props.id} onDelete={this.delete} >
				<TextFormGroup id={component_id + '-Name'} label='Name' onChange={this.nameChange} required={true}
					value={this.props.item.name} />
				<TextAreaFormGroup id={component_id + '-Description'}
					label='Description'
					onChange={this.descriptionChange}
					required={false}
					value={this.props.item.description} />
				<AttributeComponent id={component_id + '-Agility'} onChange={this.agilityChange} value={this.props.item.agility}
					prepend={<PrependAddon id={component_id + '-Agility'} >Agility</PrependAddon >} />

				<AttributeComponent id={component_id + '-Smarts'}
					prepend={<PrependAddon id={component_id + '-Smarts'} >Smarts</PrependAddon >}
					value={this.props.item.smarts}
					onChange={this.smartsChange}
					append={<div className={'input-group-append'} >
						<CheckboxFormGroup
							checked={false}
							id={component_id + '-AnimalIntelligence'} label={'Animal Intelligence'}
							value={this.props.item.animalIntelligence || false}
							onChange={this.animalIntelligenceChange} />
					</div >} />

				<AttributeComponent id={component_id + '-Spirit'}
					prepend={<PrependAddon id={component_id + '-Spirit'} >Spirit</PrependAddon >}
					value={this.props.item.spirit}
					onChange={this.spiritChange} />
				<AttributeComponent id={component_id + '-Strength'}
					prepend={<PrependAddon id={component_id + '-Strength'} >Strength</PrependAddon >}
					value={this.props.item.strength}
					onChange={this.strengthChange} />
				<AttributeComponent id={component_id + '-Vigor'}
					prepend={<PrependAddon id={component_id + '-Vigor'} >Vigor</PrependAddon >}
					value={this.props.item.vigor}
					onChange={this.vigorChange} />
				<NumberFormGroup id={component_id + '-Charisma'} label={'Charisma'} value={this.props.item.charisma}
					onChange={this.charismaChange} />
				<NumberFormGroup id={component_id + '-Pace'} label={'Pace'} value={this.props.item.pace}
					onChange={this.paceChange} />
				<SelectedHindrances id={component_id}
					hindrancesAvailable={this.props.hindrancesAvailable || []}
					hindrances={this.props.item.hindrances || []}
					onChange={this.hindranceListChanged} />
				<SelectedEdges id={component_id}
					edgesAvailable={this.props.edgesAvailable || []}
					edges={this.props.item.edges || []}
					onChange={this.edgeListChanged} />
				<SelectedSkills id={component_id}
					skillsAvailable={this.props.skillsAvailable || []}
					skills={this.props.item.skills || []}
					onChange={this.skillListChanged} />
				{this.props.children}
			</BaseEditor >
		)
	}
}

