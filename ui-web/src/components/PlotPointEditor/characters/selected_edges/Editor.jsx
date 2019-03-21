import {Button, FormControl} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import AttributeComponent from '../../AttributeComponent'

export default class Editor extends React.Component {

	static propTypes = {
		disabled         : PropTypes.bool,
		id               : PropTypes.string.isRequired,
		onChange         : PropTypes.func.isRequired,
		required         : PropTypes.bool,
		valid            : PropTypes.bool,
		validationMessage: PropTypes.string,

		edgesAvailable: PropTypes.array.isRequired,
		edge          : PropTypes.shape({
			name: PropTypes.string,
			note: PropTypes.string
		}).isRequired
	}

	static defaultProps = {}

	delete      = e => {
		e.preventDefault()
		this.props.onDelete(this.props.index)
	}
	noteChanged = e => this.props.onChange(this.props.index, Object.assign({}, this.props.edge, {note: e.target.value}))

	render() {
		let {disabled, id, required} = this.props
		let componentId              = `SelectedSkillEditor-${id}`
		let append                   = <div id={'AppendAddon-' + componentId} className={'input-group-append'}>
			<Button id={componentId + '-Delete'} onClick={this.delete}>Delete</Button>
		</div>
		let prepend                  = <div id={'prepend-' + componentId}
		                                    className={'input-group-prepend'}><select
			className={'form-control'} disabled={disabled} id={componentId} onChange={this.nameChanged}
			required={required} value={this.props.edge.name}>
			{this.props.edgesAvailable.map((o, index) => <option key={index} value={o.value}>{o.label}</option>)}
		</select></div>
		return <AttributeComponent append={append} id={componentId} prepend={prepend} onChange={this.diceChanged}
		                           value={this.props.edge.rank}>
			<FormControl cssClass='form-control' id={componentId} type={'text'} onChange={this.noteChanged}
			             placeholder={'Specialization'} value={this.props.edge.note}/>
		</AttributeComponent>
	}
}
