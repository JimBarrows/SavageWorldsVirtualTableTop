import {NumberFormGroup, SelectFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import DiceSelectFormGroup from '../formgroups/DiceSelectFormGroup'

export default class SelectedSkillEditor extends React.Component {

  static propTypes = {
    id: PropTypes.string.isRequired,
    onChange: PropTypes.func.isRequired,
    skillsAvailable: PropTypes.array.isRequired,
    skill: PropTypes.object.isRequired
  };

  static defaultProps = {};

  bonusChanged = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {bonus: e.target.value}));
  diceChanged  = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {dice: e.target.value}));
  noteChanged  = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {note: e.target.value}));
  nameChanged  = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {name: e.target.value}));

  render() {
    let id = `selectedSkillEditor_${this.props.id}`;
    console.log('editor: ', this.props.skill);
    return (<div id={'SelectedSkillEditorComponent_' + this.props.id} className={'row'}>
      <div className={'col-sm-3'}>
        <SelectFormGroup id={id} onChange={this.nameChanged} options={this.props.skillsAvailable} value={this.props.skill.name}/>
      </div>
      <div className={'col-sm-2'}>
        <DiceSelectFormGroup id={id} onChange={this.diceChanged} value={this.props.skill.dice}/>
      </div>
      <div className={'col-sm-1'}>
        <NumberFormGroup id={id} onChange={this.bonusChanged} value={this.props.skill.bonus}/>
      </div>
      <div className={'col-sm-5'}>
        <TextFormGroup id={id} onChange={this.noteChanged} value={this.props.skill.note}/>
      </div>
      <div className={'col-sm-1'}>
        <button type={'button'} className={'btn btn-default'} onChange={this.save}>Save</button>
      </div>
    </div>);
  }
}
