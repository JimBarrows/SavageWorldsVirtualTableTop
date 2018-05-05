import {
	Panel,
	PanelBody,
	PanelHeader,
	RemoveButton,
	TextAreaFormGroup,
	TextFormGroup
} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';
import RaceAbilityEditor from './RaceAbilityEditor';

export default class RaceEditor extends React.Component {

	static propTypes = {
		race    : PropTypes.object,
		index   : PropTypes.number.isRequired,
		onChange: PropTypes.func.isRequired
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
							<RemoveButton id={'raceEditor'} onClick={this.onDelete}/>
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
								                   onDelete={this.onAbilityDelete}/>)}
					</PanelBody>
				</Panel>

		);
	}
}