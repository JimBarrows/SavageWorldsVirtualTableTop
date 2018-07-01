import {SelectFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'

export default class DiceSelectFormGroup extends React.Component {

	static propTypes = {
		dice    : PropTypes.string,
		id      : PropTypes.string.isRequired,
		label   : PropTypes.string.isRequired,
		onChange: PropTypes.func.isRequired
	};

	static defaultProps = {
		label   : ' ',
		required: false
	};

	render() {
		return (
				<div id={'DiceFormGroupComponent_' + this.props.id}>
					<SelectFormGroup id={'DiceFormGroupComponent_' + this.props.id}
					                 label={this.props.label}
					                 onChange={this.props.onChange}
					                 options={[{label: 'd4', value: 'd4'},
						                 {label: 'd6', value: 'd6'},
						                 {label: 'd8', value: 'd8'},
						                 {label: 'd10', value: 'd10'},
						                 {label: 'd12', value: 'd12'}]}
					                 required={this.props.required}
					                 value={this.props.value}/>
				</div>
		);
	}
}
