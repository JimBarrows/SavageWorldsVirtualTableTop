import React from 'react';
import PropTypes from 'prop-types';
import {
	Panel,
	PanelBody,
	PanelHeader,
	RemoveButton,
	TextAreaFormGroup,
	TextFormGroup
} from 'bootstrap-react-components';
import RaceAbilityEditor from './RaceAbilityEditor';

export default class RaceEditor extends React.Component {

	static propTypes = {
		addRacialAbility   : PropTypes.func.isRequired,
		description        : PropTypes.string,
		index              : PropTypes.number.isRequired,
		name               : PropTypes.string,
		onChange           : PropTypes.func.isRequired,
		deleteRacialAbility: PropTypes.func.isRequired
	};

	addRacialAbility = (e) => {
		e.preventDefault();
		this.props.addRacialAbility(this.props.index);
	};

	deleteRacialAbility = (racialAbilityIndex) => {
		this.props.deleteRacialAbility(this.props.index, racialAbilityIndex);
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
			description: this.props.race.description,
			abilities  : this.props.race.abilities
		}, this.props.index);
	};

	onDescriptionChange = event => {
		this.props.onChange({
			name       : this.props.race.name,
			description: event.target.value,
			abilities  : this.props.race.abilities
		}, this.props.index);
	};

	render() {
		const {abilities, name, description, index} = this.props.race;
		const {descriptionError, nameError}         = this.props;

		return (
				<Panel id={'raceEditor'}>
					<PanelHeader id={'raceEditor'}>
						<div className={'btn-group pull-right'}>
							<RemoveButton id={'raceEditor'} onClick={this.delete}/>
						</div>
					</PanelHeader>
					<PanelBody id={'raceEditor'}>
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
								                   onDelete={this.deleteRacialAbility}/>)}
					</PanelBody>
				</Panel>

		);
	}
}