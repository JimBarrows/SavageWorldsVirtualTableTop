import {
	NumberFormGroup,
	Panel,
	PanelBody,
	PanelHeader,
	RemoveButton,
	TextAreaFormGroup,
	TextFormGroup
} from "bootstrap-react-components";
import PropTypes from 'prop-types';
import React from "react";

class RaceAbilityEditor extends React.Component {

	static propTypes = {
		ability : PropTypes.object.isRequired,
		onChange: PropTypes.func.isRequired,
		onDelete: PropTypes.func.isRequired
	};

	costChange = event => this.props.onChange({
		cost       : parseInt(event.target.value, 10),
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

	onDelete = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};

	render() {
		let {cost, description, name} = this.props.ability;
		return (
				<Panel id={'RaceAbilityEditor'}>
					<PanelHeader id={'RaceAbilityEditor'}>
						<div className={'btn-group pull-right'}>
							<RemoveButton id={'RaceAbilityEditor'} onClick={this.onDelete}/>
						</div>
					</PanelHeader>
					<PanelBody id={'raceEditor'}>
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
					</PanelBody>
				</Panel>
		);
	}


}

export default RaceAbilityEditor;