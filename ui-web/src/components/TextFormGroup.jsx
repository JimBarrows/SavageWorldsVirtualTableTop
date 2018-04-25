import {FormGroup} from 'bootstrap-react-components';
import PropTypes from 'prop-types'
import React from 'react'

export default class TextFormGroup extends React.Component {

	render () {
		let {disabled, error, id, label, onChange, placeholder, required, value} = this.props
		return (
				<FormGroup label={label} id={id} error={error} required={required} >
					<input className='form-control' disabled={disabled} id={id} onChange={onChange} placeholder={placeholder}
					       required={required} type='text' value={value} />
				</FormGroup >
		)
	}
}

TextFormGroup.defaultProps = {
	disabled: false,
	required: false
}

TextFormGroup.propTypes = {
	disabled: PropTypes.bool,
	error: PropTypes.string,
	id: PropTypes.string.isRequired,
	label: PropTypes.string,
	onChange: PropTypes.func.isRequired,
	placeholder: PropTypes.string,
	required: PropTypes.bool,
	value: PropTypes.string
}
