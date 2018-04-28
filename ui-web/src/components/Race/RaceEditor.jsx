import React from 'react';
import PropTypes from 'prop-types';
import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components';
import RaceAbilityEditor from './RaceAbilityEditor';

export default class RaceEditor extends React.Component {

	static propTypes = {
		addRacialAbility: PropTypes.func.isRequired,
		description     : PropTypes.string,
		index           : PropTypes.number.isRequired,
		name            : PropTypes.string,
		onChange        : PropTypes.func.isRequired
	};

	addRacialAbility = (e) => {
		e.preventDefault();
		this.props.addRacialAbility(this.props.index);
	};

	onAbilityChange = (ability, index) => {
		this.props.onChange({
			name       : this.props.race.name,
			description: this.props.race.description,
			abilities  : this.props.race.abilities.map((a, idx) => index === idx ? ability : a)
		}, this.props.index);
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
		const {abilities, name, description, index} = this.props.race;
		const {descriptionError, nameError}         = this.props;

		return (
				<div id='raceEditor'>
					<TextFormGroup
							error={nameError}
							label='Race'
							id={'raceFormName_' + index}
							onChange={this.onNameChange}
							required={true}
							value={name}
					/>
					<TextAreaFormGroup
							error={descriptionError}
							label='Description'
							id={'raceFormDescription_' + index}
							onChange={this.onDescriptionChange}
							required={true}
							value={description}
					/>
					<h3>Racial Abilities</h3>
					<button className="btn btn-default" onClick={this.addRacialAbility}>Add</button>
					{abilities.map((ability, index) =>
							<RaceAbilityEditor key={index} index={index} ability={ability}
							                   onChange={this.onAbilityChange}/>)}
				</div>
		)
				;
	}
}