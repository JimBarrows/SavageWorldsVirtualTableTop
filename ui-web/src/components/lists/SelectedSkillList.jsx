import PropTypes from 'prop-types'
import React from 'react'
import SelectedSkillEditor from '../editors/SelectedSkillEditor'

export default class SelectedSkillList extends React.Component {

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

  skillDeleted = indexOfSkill => this.props.onChange(this.props.skills.splice(indexOfSkill, 1))

  skillEditorList = () => this.props.skills.map((selectedSkill, index) => <SelectedSkillEditor key={index}
                                                                                               id={'skill_' + index}
                                                                                               index={index}
                                                                                               onChange={this.skillChanged}
                                                                                               onDelete={this.skillDeleted}
                                                                                               skill={selectedSkill}
                                                                                               skillsAvailable={this.props.skillsAvailable}/>)

  render() {
    return (
      <div id={'SelectedSkillListComponent_' + this.props.id}>
        <h3>Skills</h3>
        <button id={`addSelectedSkillButton+_${this.props.id}`} className="btn btn-default"
                onClick={this.addSkill}>Add
        </button>
        {this.skillEditorList()}
      </div>
    )
  }
}
