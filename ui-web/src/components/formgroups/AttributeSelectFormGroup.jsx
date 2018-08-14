import {SelectFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'

export default class AttributeSelectFormGroup extends React.Component {

	static propTypes = {
		attribute: PropTypes.string,
		id       : PropTypes.string.isRequired,
		label    : PropTypes.string.isRequired,
		onChange : PropTypes.func.isRequired
	};

	static defaultProps = {
		label: 'Attribute'
	};

	render() {
		return (
			<div id={'AttributeSelectFormGroup-' + this.props.id}>
					<SelectFormGroup id={this.props.id}
					                 label={this.props.label}
					                 onChange={this.props.onChange}
					                 options={[{label: 'Agility', value: 'Agility'},
						                 {label: 'Smarts', value: 'Smarts'},
						                 {label: 'Spirit', value: 'Spirit'},
						                 {label: 'Strength', value: 'Strength'},
						                 {label: 'Vigor', value: 'Vigor'}]}
					                 value={this.props.attribute}/>
				</div>
		);
	}
}
