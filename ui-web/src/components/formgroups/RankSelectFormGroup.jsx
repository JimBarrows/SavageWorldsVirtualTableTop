import {SelectFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'

export default class RankSelectFormGroup extends React.Component {

	static propTypes = {
		rank    : PropTypes.string,
		id      : PropTypes.string.isRequired,
		label   : PropTypes.string.isRequired,
		onChange: PropTypes.func.isRequired,
		required: PropTypes.bool
	};

	static defaultProps = {
		label   : 'Rank',
		required: false
	};

	render() {
		return (
				<div id={'RankFormGroupComponent_' + this.props.id}>
					<SelectFormGroup id={'RankFormGroupComponent_' + this.props.id}
					                 label={this.props.label}
					                 onChange={this.props.onChange}
					                 options={[{label: 'Novice', value: 'Agility'},
						                 {label: 'Seasoned', value: 'Seasoned'},
						                 {label: 'Veteran', value: 'Veteran'},
						                 {label: 'Heroic', value: 'Heroic'},
						                 {label: 'Legendary', value: 'Legendary'}]}
					                 required={this.props.required}
					                 value={this.props.rank}/>
				</div>
		);
	}
}
