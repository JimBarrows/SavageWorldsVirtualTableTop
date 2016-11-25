'use strict';
import React from "react";
import * as Action from "../actions/PlotPointActions";
import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";


export default class RaceEditor extends React.Component {

	cancel() {

	}

	constructor(props) {
		super(props);
		let {_id, name, description, abilities, descriptionError, abilityError, nameError, onChange} = props;
		if (!abilities) {
			abilities = [];
		}
		this.state = {
			_id, name, description, abilities, descriptionError, abilityError, nameError, onChange
		};
	}

	onChange(event) {
		switch (event.target.id) {
			case 'raceFormName' :
				this.setState({
					name: event.target.value
				});
				break;
			case "raceFormDescription" :
				this.setState({
					description: event.target.value
				});
				break;
		}
	}

	render() {
		const {_id, name, description, abilities, descriptionError, abilityError, nameError} = this.state;
		const {save}                                                                         = this.props;
		return (
				<div id="raceForm">
					<TextFormGroup
							error={nameError}
							label="Race"
							id="raceFormName"
							onChange={this.onChange.bind(this)}
							value={name}
					/>
					<TextAreaFormGroup
							error={descriptionError}
							label="Description"
							id="raceFormDescription"
							onChange={this.onChange.bind(this)}
							value={description}
					/>
					<h3>Abilities</h3>
					{abilities.map((ability, index) => (
							<div key={index} class="ability">
								<h4 ><TextFormGroup
										error={abilityError}
										label="Ability Name"
										id={"abilityName" + index}
										onChange={this.onChange.bind(this)}
										value={ability.name}/>
									<small>
										<bold>Cost:</bold>
										<NumberFormGroup error={costError} id={"abilityCost" + index} onChange={this.onChange.bind(this)}
										                 value={ability.cost}/>
								</small>
							</h4>
						{ability.description}
						</div>))}
					<button type="button" class="btn btn-primary"
					        onClick={this.save.bind(this)}>
						Save
					</button>
					<button type="button" class="btn btn-default" onClick={this.cancel.bind(this)}>Cancel</button>
				</div>
		)
	}

	save(event) {
		event.preventDefault();
		let {_id, name, description, abilities} = this.state;
		Action.addRace({
			_id,
			name,
			description,
			abilities
		});
	}
}