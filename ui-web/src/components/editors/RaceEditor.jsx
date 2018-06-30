import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import BaseEditor from './BaseEditor'
import RaceAbilityEditor from './RaceAbilityEditor'

export default class RaceEditor extends React.Component {

	static defaultProps = {
		id: 'RaceEditor'
	};

	addRacialAbility = (e) => {
		e.preventDefault();
		this.props.onChange({
			description: this.props.item.description,
			name       : this.props.item.name,
			abilities  : [{name: '', description: '', cost: 0}, ...this.props.item.abilities]
		}, this.props.index);
	};

	onAbilityChange = (ability, index) => {
		this.props.onChange({
			name       : this.props.item.name,
			description: this.props.item.description,
			abilities  : this.props.item.abilities.map((a, idx) => index === idx ? ability : a)
		}, this.props.index);
	};

	onAbilityDelete = index => {
		this.props.onChange({
			name       : this.props.item.name,
			description: this.props.item.description,
			abilities  : this.props.item.abilities.filter((a, idx) => index !== idx)
		}, this.props.index);
	};

	onNameChange = event => {
		this.props.onChange({
			name       : event.target.value,
			description: this.props.item.description,
			abilities  : this.props.item.abilities
		}, this.props.index);
	};

	onDelete            = () => {
		this.props.onDelete(this.props.index);
	};
	onDescriptionChange = event => {
		this.props.onChange({
			name       : this.props.item.name,
			description: event.target.value,
			abilities  : this.props.item.abilities
		}, this.props.index);
	};

	render() {
		let {abilities, name, description} = this.props.item;
		let {index}                        = this.props;
		let {descriptionError, nameError}  = this.props;

		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup
							error={nameError}
							label='Race'
							id={'itemFormName_' + index}
							onChange={this.onNameChange}
							required={true}
							value={name}
					/>
					<TextAreaFormGroup
							error={descriptionError}
							label='Description'
							id={'itemFormDescription_' + index}
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