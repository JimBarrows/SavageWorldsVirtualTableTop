import {Button} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import SelectedSkillEditor from '../editors/SelectedSkillEditor'

export default class SelectedSkills extends React.Component {

	static propTypes = {
		id             : PropTypes.string.isRequired,
		skillsAvailable: PropTypes.array.isRequired,
		skills         : PropTypes.array.isRequired,
		onChange       : PropTypes.func.isRequired
	}

	static defaultProps = {}

	addSkill = (e) => {
		e.preventDefault()
		this.props.onChange([{name: ' ', rank: {dice: 'd4', bonus: 0}, note: ' '}, ...this.props.skills])
	}

	skillChanged = (indexOfChange, changedSkill) => {
		this.props.onChange(this.props.skills.map((skill, index) => indexOfChange === index ? changedSkill : skill))
	}

	skillDeleted = indexOfSkill => {
		this.props.skills.splice(indexOfSkill, 1)
		this.props.onChange(this.props.skills)
	}

	skillEditorList = () => this.props.skills.map((selectedSkill, index) => <SelectedSkillEditor key={index}
	                                                                                             id={'skill_' + index}
	                                                                                             index={index}
	                                                                                             onChange={this.skillChanged}
	                                                                                             onDelete={this.skillDeleted}
	                                                                                             skill={selectedSkill}
	                                                                                             skillsAvailable={this.props.skillsAvailable}/>)

	render() {
		let componentId = `SelectedSkillList-${this.props.id}`

		return <div id={componentId}>
			<h3>Skills</h3>
			<Button id={`${componentId}-AddButton`} onClick={this.addSkill}>Add</Button>
			{this.skillEditorList()}
		</div>
	}
}
