import React from 'react';
import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components';
import AbilityList from '../Ability/List';
import {ItemEditor} from '../Item';

export default class RaceEditor extends ItemEditor {

	onAbilityListChange(list) {
		this.setState({
			abilities: list
		});
	}

	propsToState(props) {
		let {_id, name, description, abilities = [], descriptionError, abilityError, nameError} = props;
		this.setState({
			_id, name, description, abilities, descriptionError, abilityError, nameError
		});
	}

	render() {
		const {name, description, abilities, descriptionError, nameError} = this.state;

		return (
				<div id='raceForm'>
					<TextFormGroup
							error={nameError}
							label='Race'
							id='raceFormName'
							onChange={this.nameChange.bind(this)}
							value={name}
					/>
					<TextAreaFormGroup
							error={descriptionError}
							label='Description'
							id='raceFormDescription'
							onChange={this.descriptionChange.bind(this)}
							value={description}
					/>
					<AbilityList list={abilities} onListChange={this.onAbilityListChange.bind(this)} allowEditing={true}/>
					<button type='button' class='btn btn-default'
					        onClick={this.save.bind(this)}>Save
					</button>
					<button type='button' class='btn btn-default' onClick={this.cancel.bind(this)}>Cancel</button>
				</div>
		)
	}

	stateToItem() {
		let {_id, name, description, abilities} = this.state;
		return {_id, name, description, abilities};
	}
}