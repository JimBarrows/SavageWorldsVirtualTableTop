import {TextFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import AttributeFormGroup from '../formgroups/AttributeFormGroup'

export default class SelectedSkillEditor extends React.Component {

  static propTypes = {
    id             : PropTypes.string.isRequired,
    onChange       : PropTypes.func.isRequired,
    skillsAvailable: PropTypes.array.isRequired,
    skill          : PropTypes.object.isRequired
  }

  static defaultProps = {}

  bonusChanged = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {bonus: e.target.value}))
  diceChanged  = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {dice: e.target.value}))
  noteChanged  = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {note: e.target.value}))
  nameChanged  = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {name: e.target.value}))

  render() {
    let id = `SelectedSkillEditor-${this.props.id}`
    return (<div id={'SelectedSkillEditorComponent_' + this.props.id} className={'row'}>
      <AttributeFormGroup id={id} onChange={this.props.onChange}/>
      <TextFormGroup id={id} onChange={this.noteChanged} value={this.props.skill.note}/>
      <button type={'button'} className={'btn btn-default'} onChange={this.save}>Save</button>
    </div>)
  }
}
