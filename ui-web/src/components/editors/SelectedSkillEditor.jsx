import {Button, FormControl} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import AttributeComponent from '../AttributeComponent'

export default class SelectedSkillEditor extends React.Component {

	static propTypes = {
		disabled         : PropTypes.bool,
		id               : PropTypes.string.isRequired,
		onChange         : PropTypes.func.isRequired,
		required         : PropTypes.bool,
		valid            : PropTypes.bool,
		validationMessage: PropTypes.string,

		skillsAvailable: PropTypes.array.isRequired,
		skill          : PropTypes.shape({
			name: PropTypes.string,
			rank: PropTypes.shape({
				dice : PropTypes.oneOf(['d4', 'd6', 'd8', 'd10', 'd12']).isRequired,
				bonus: PropTypes.number,
			}).isRequired,
			note: PropTypes.string
		}).isRequired
	}

	static defaultProps = {}

	delete      = e => {
		e.preventDefault()
		this.props.onDelete(this.props.index)
	}
	diceChanged = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {rank: e}))
	noteChanged = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {note: e.target.value}))
	nameChanged = e => this.props.onChange(this.props.index, Object.assign({}, this.props.skill, {name: e.target.value}))

	render() {
		let {disabled, id, required, valid, validationMessage} = this.props
		let componentId                                        = `SelectedSkillEditor-${id}`
		let append                                             = <div id={'AppendAddon-' + componentId}
		                                                              className={'input-group-append'}>
			<Button id={componentId + '-Delete'} onClick={this.delete}>Delete</Button>


		</div>
		let prepend                                            = <div id={'prepend-' + componentId}
		                                                              className={'input-group-prepend'}><select
			className={'form-control'} disabled={disabled} id={componentId} onChange={this.nameChanged}
			required={required} value={this.props.skill.name}>
			{this.props.skillsAvailable.map((o, index) => <option key={index} value={o.value}>{o.label}</option>)}
		</select></div>
		return <AttributeComponent append={append} id={componentId} prepend={prepend} onChange={this.diceChanged}
		                           value={this.props.skill.rank}>
			<FormControl cssClass='form-control' id={componentId} type={'text'} onChange={this.noteChanged}
			             placeholder={'Specialization'} type={'text'} value={this.props.skill.note}/>
		</AttributeComponent>
	}
}
