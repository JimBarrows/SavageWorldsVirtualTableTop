import {SelectFormGroup} from 'bootstrap-react-components'
import PropTypes         from 'prop-types'
import React             from 'react'

export default class DiceSelect extends React.Component {

	static defaultProps = {
		disabled: false,
		required: false,
		value   : 'd4'
	}

	static propTypes = {
		disabled: PropTypes.bool,
		id      : PropTypes.string.isRequired,
		label   : PropTypes.string,
		onChange: PropTypes.func.isRequired,
		required: PropTypes.bool,
		value   : PropTypes.oneOf(['', 'd4', 'd6', 'd8', 'd10', 'd12']),
	}

	options = [
		{label: 'd4', value: 'd4'},
		{label: 'd6', value: 'd6'},
		{label: 'd8', value: 'd8'},
		{label: 'd10', value: 'd10'},
		{label: 'd12', value: 'd12'}
	]

	render () {
		let {disabled, id, onChange, required, value} = this.props
		return (
			<SelectFormGroup disabled={disabled} id={'DiceSelect-' + id} onChange={onChange}
				required={required} value={value} options={this.options} />
		)
	}
}

