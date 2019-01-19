import {FormGroup} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';

export default class SelectFormGroup extends React.Component {

	static defaultProps = {
		disabled: false,
		required: false
	};

	static propTypes = {
		disabled: PropTypes.bool,
		error   : PropTypes.string,
		id      : PropTypes.string.isRequired,
		label   : PropTypes.string,
		onChange: PropTypes.func.isRequired,
		options : PropTypes.array.isRequired,
		required: PropTypes.bool,
		value   : PropTypes.string.isRequired
	};

	render() {
		let {disabled, error, id, label, onChange, options, required, value} = this.props;
		return (
				<FormGroup label={label} id={id} error={error} required={required}>
					<select className='form-control' disabled={disabled} id={id} onChange={onChange}
					        required={required} value={value}>
						<option value='-1'/>
						{options.map((o, index) => <option key={index}
						                                   value={o.value}>{o.label}</option>)}
					</select>
				</FormGroup>
		);
	}
}

