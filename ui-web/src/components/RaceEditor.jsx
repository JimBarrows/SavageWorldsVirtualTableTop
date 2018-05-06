import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';
import BaseEditor from './BaseEditor';
import RaceAbilityEditor from './RaceAbilityEditor';

export default class RaceEditor extends React.Component {

	static propTypes = {
		race    : PropTypes.object,
		index   : PropTypes.number.isRequired,
		onChange: PropTypes.func.isRequired
	};

	static defaultProps = {
		id: 'RaceEditor'
	};

	addRacialAbility = (e) => {
		e.preventDefault();
		this.props.onChange({
			description: this.props.race.description,
			name       : this.props.race.name,
			abilities  : [{name: '', description: '', cost: 0}, ...this.props.race.abilities]
		}, this.props.index);
	};

	onAbilityChange = (ability, index) => {
		this.props.onChange({
			name       : this.props.race.name,
			description: this.props.race.description,
			abilities  : this.props.race.abilities.map((a, idx) => index === idx ? ability : a)
		}, this.props.index);
	};

	onAbilityDelete = index => {
		this.props.onChange({
			name       : this.props.race.name,
			description: this.props.race.description,
			abilities  : this.props.race.abilities.filter((a, idx) => index !== idx)
		}, this.props.index);
	};

	onNameChange = event => {
		this.props.onChange({
			name       : event.target.value,
			description: this.props.race.description,
			abilities  : this.props.race.abilities
		}, this.props.index);
	};

	onDelete            = () => {
		this.props.onDelete(this.props.index);
	};
	onDescriptionChange = event => {
		this.props.onChange({
			name       : this.props.race.name,
			description: event.target.value,
			abilities  : this.props.race.abilities
		}, this.props.index);
	};

	render() {
		let {abilities, name, description} = this.props.race;
		let {index}                        = this.props;
		let {descriptionError, nameError}  = this.props;

		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
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
					<button id={'addRacialAbilityButton'} className="btn btn-default" onClick={this.addRacialAbility}>Add
					</button>
					{abilities.map((ability, index) =>
							<RaceAbilityEditor ability={ability} index={index} key={index}
							                   onChange={this.onAbilityChange}
							                   onDelete={this.onAbilityDelete}/>)}
				</BaseEditor>

		);
	}
}