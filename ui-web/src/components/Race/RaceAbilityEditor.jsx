import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import React from "react";

class RaceAbilitiyEditor extends React.Component {

	costChange = event => this.props.onChange({
		cost       : event.target.value,
		description: this.props.ability.description,
		name       : this.props.ability.name
	}, this.props.index);

	descriptionChange = event => this.props.onChange({
		cost       : this.props.ability.cost,
		description: event.target.value,
		name       : this.props.ability.name
	}, this.props.index);

	nameChange = event => this.props.onChange({
		cost       : this.props.ability.cost,
		description: this.props.ability.description,
		name       : event.target.value
	}, this.props.index);

	render() {
		let {cost, description, name} = this.props.ability;
		return (
				<div id="RaceAbilityEditorForm">
					<TextFormGroup id={"abilityName"}
					               label="Ability Name"
					               onChange={this.nameChange}
					               value={name}/>
					<NumberFormGroup id={"cost"}
					                 label="Cost"
					                 onChange={this.costChange}
					                 value={cost}/>
					<TextAreaFormGroup id={"abilityDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={description}
					/>
				</div>
		);
	}


}

export default RaceAbilitiyEditor;