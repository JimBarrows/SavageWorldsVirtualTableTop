import React from "react";
import {TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import AbilityList from "../Ability/List";

export default class RaceEditor extends React.Component {

	cancel() {

	}

	componentWillMount() {
		this.propsToState(this.props);

	}

	componentWillReceiveProps(nextProps) {
		this.propsToState(nextProps);
	}

	constructor(props) {
		super(props);
	}

	descriptionChange(e) {
		this.setState({
			description: e.target.value
		});
	}

	nameChange(e) {
		this.setState({
			name: e.target.value
		});
	}

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
				<div id="raceForm">
					<TextFormGroup
							error={nameError}
							label="Race"
							id="raceFormName"
							onChange={this.nameChange.bind(this)}
							value={name}
					/>
					<TextAreaFormGroup
							error={descriptionError}
							label="Description"
							id="raceFormDescription"
							onChange={this.descriptionChange.bind(this)}
							value={description}
					/>
					<AbilityList list={abilities} onListChange={this.onAbilityListChange.bind(this)} allowEditing={true}/>
					<button type="button" class="btn btn-default"
					        onClick={this.save.bind(this)}>Save
					</button>
					<button type="button" class="btn btn-default" onClick={this.cancel.bind(this)}>Cancel</button>
				</div>
		)
	}

	save(event) {
		event.preventDefault();
		let {_id, name, description, abilities} = this.state;
		this.props.save({
			_id, name, description, abilities
		})
	}
}