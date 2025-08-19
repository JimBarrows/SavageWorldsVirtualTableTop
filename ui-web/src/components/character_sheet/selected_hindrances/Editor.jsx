import {TextAreaFormGroup} from 'bootstrap-react-components'
import PropTypes           from 'prop-types'
import React               from 'react'

export default class SelectedHindranceEditor extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : PropTypes.string.isRequired,
		hindrance: PropTypes.object.isRequired,
		onChange : PropTypes.func
	}

	handleSeverityChange = (severity) => {
		const { hindrance, onChange } = this.props;
		if (onChange) {
			onChange({
				...hindrance,
				severity: severity
			});
		}
	}

	severity = () => {
		let {hindrance} = this.props
		if (hindrance.hindrance.severity === 'Major or Minor') {
			return <div >
				<label >
					<input 
						type="radio" 
						name={`severity-${this.props.id}`}
						value="Major"
						checked={hindrance.severity === 'Major'}
						onChange={() => this.handleSeverityChange('Major')}
					/>
					Major
				</label >
				<label >
					<input 
						type="radio" 
						name={`severity-${this.props.id}`}
						value="Minor"
						checked={hindrance.severity === 'Minor'}
						onChange={() => this.handleSeverityChange('Minor')}
					/> 
					Minor
				</label >
			</div >
		} else {
			const severity = hindrance.severity || hindrance.hindrance.severity;
			return <label >Severity: {severity}</label >
		}
	}

	hindranceNoteChange = (e) => {
		const { hindrance, onChange } = this.props;
		if (onChange) {
			onChange({
				...hindrance,
				note: e.target.value
			});
		}
	}

	render () {
		let {hindrance, id} = this.props
		let component_id    = `SelectedHindranceEditor-${id}`
		return (
			<div id={component_id} >
				<h4 >{hindrance.hindrance.name}</h4 >
				{this.severity()}
				{hindrance.hindrance.description}
				<TextAreaFormGroup id={`${component_id}-Note`} label={'Note'}
					onChange={this.hindranceNoteChange}
					value={hindrance.note} />
			</div >
		)
	}
}

