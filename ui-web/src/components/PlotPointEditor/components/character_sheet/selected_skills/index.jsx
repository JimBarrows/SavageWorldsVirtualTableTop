import {Button, PrependAddon, SelectFormGroup} from 'bootstrap-react-components'
import PropTypes                               from 'prop-types'
import React                                   from 'react'
import AttributeComponent                      from '../../AttributeComponent'

export default class Index extends React.Component {

	static propTypes        = {
		id             : PropTypes.string.isRequired,
		skillsAvailable: PropTypes.array.isRequired,
		skills         : PropTypes.array.isRequired,
		onChange       : PropTypes.func.isRequired
	}
	static defaultProps     = {}
				 attributeChange  = (skill, index) => e => {
					 skill.rank = Object.assign({}, skill.rank, e)
					 this.props.onChange(this.props.skills)
				 }
				 addSkill         = e => {
					 this.props.onChange([...this.props.skills, {
						 skill: this.state.selectedSkill,
						 rank : {dice: 'd4', bonus: null},
						 note : ''
					 }].sort((l, r) => l.skill.name < r.skill.name ? -1 : l.skill.name < r.skill.name ? 1 : 0))
					 this.setState({selectedSkill: null})
				 }
				 newSkillSelected = e => this.setState({selectedSkill: this.props.skillsAvailable.find(skill => skill.name === e.target.value)})

	render () {
		const component_id     = `SelectedSkillList-${this.props.id}`
		const skill_names      = this.props.skills.map(s => s.skill.name)
		const skills_remaining = this.props.skillsAvailable
			.filter(skill => !skill_names.includes(skill.name))
			.map(skill => ({
				label: skill.name,
				value: skill.name
			}))
		return <div id={component_id} >
			<h3 >Skills</h3 >
			<SelectFormGroup id={component_id} label={'Skills'} onChange={this.newSkillSelected} options={skills_remaining} />
			<Button id={`${component_id}-AddButton`} onClick={this.addSkill} >Add</Button >
			{
				this.props.skills.map((skill, index) =>
																<div key={index} >
																	<AttributeComponent id={`${component_id}-${index}`}
																		onChange={this.attributeChange(skill, index)}
																		value={skill.rank}
																		prepend={<PrependAddon
																			id={`${component_id}-${index}`} >{skill.skill.name} ({skill.skill.attribute})</PrependAddon >} >
																		{skill.note}
																	</AttributeComponent >

																</div >
				)
			}
		</div >
	}

	state = {
		selectedSkill: null
	}
}
