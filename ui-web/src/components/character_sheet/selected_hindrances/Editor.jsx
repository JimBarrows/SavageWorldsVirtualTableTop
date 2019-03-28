import {TextAreaFormGroup} from 'bootstrap-react-components'
import PropTypes           from 'prop-types'
import React               from 'react'

export default class SelectedHindranceEditor extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : PropTypes.string.isRequired,
		hindrance: PropTypes.object.isRequired
	}

	severity = () => {
		let {hindrance} = this.props
		if (hindrance.hindrance.severity === 'Major or Minor') {
			return <div >
				<label ><input type="radio" name="severity" value="Major" />Major</label >
				<label ><input type="radio" name="severity" value="Minor" /> Minor</label >
			</div >
		} else {
			hindrance.severity = hindrance.hindrance.severity
			return <label >Severity: {hindrance.serverity}</label >
		}
	}

	hindranceNoteChange = hindrance => e => hindrance.note = e.target.value

	render () {
		let {hindrance, id} = this.props
		let component_id    = `SelectedHindranceEditor-${id}`
		return (
			<div id={component_id} >
				<h4 >{hindrance.hindrance.name}</h4 >
				{this.severity()}
				{hindrance.hindrance.description}
				<TextAreaFormGroup id={`${component_id}-Note`} label={'Note'}
					onChange={this.hindranceNoteChange(hindrance)}
					value={hindrance.note} />
			</div >
		)
	}
}

