import React from 'react';
import PropTypes from 'prop-types';
import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components';

export default class RaceEditor extends React.Component {

	static propTypes = {
		description: PropTypes.string,
		index      : PropTypes.number.isRequired,
		name       : PropTypes.string,
		onChange   : PropTypes.func.isRequired
	};

	onNameChange = event => {
		this.props.onChange({
			name       : event.target.value,
			description: this.props.race.description
		}, this.props.index);
	};

	onDescriptionChange = event => {
		this.props.onChange({
			name       : this.props.race.name,
			description: event.target.value
		}, this.props.index);
	};

	render() {
		const {name, description,}          = this.props.race;
		const {descriptionError, nameError} = this.props;

		return (
				<div id='raceEditor'>
					<TextFormGroup
							error={nameError}
							label='Race'
							id='raceFormName'
							onChange={this.onNameChange}
							value={name}
					/>
					<TextAreaFormGroup
							error={descriptionError}
							label='Description'
							id='raceFormDescription'
							onChange={this.onDescriptionChange}
							value={description}
					/>
				</div>
		);
	}
}