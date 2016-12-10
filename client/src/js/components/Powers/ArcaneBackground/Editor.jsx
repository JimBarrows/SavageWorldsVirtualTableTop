import {NumberFormGroup, TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import React from "react";
import {ItemEditor} from "../../Item";
import SkillSelect from "../../SkillSelect";

class ArcaneEditor extends ItemEditor {

	propsToState(props) {
		let {_id, name, description, skill, startingPowerPoints, startingPowers} = props;
		this.setState({
			_id, name, description, skill, startingPowerPoints, startingPowers
		});
	}

	startingPowerPointsChange(e) {
		this.setState({
			startingPowerPoints: e.target.value
		});
	}

	startingPowersChange(e) {
		this.setState({
			startingPowers: e.target.value
		});
	}

	render() {
		let {_id, name, description, skill, startingPowerPoints, startingPowers} = this.state;
		return (
				<div id="ArcaneEditorForm">
					<TextFormGroup
							label="Name"
							onChange={this.nameChange.bind(this)}
							value={name}
							required={true}/>
					<SkillSelect value={skill} onChange={this.skillChange.bind(this)}/>
					<NumberFormGroup value={startingPowerPoints} label="Starting Power Points" required={true}
					                 onChange={this.startingPowerPointsChange.bind(this)}/>
					<NumberFormGroup value={startingPowers} label="Starting Powers" required={true}
					                 onChange={this.startingPowersChange.bind(this)}/>
					<TextAreaFormGroup
							label="Description"
							onChange={this.descriptionChange.bind(this)}
							value={description}
					/>
					<button type="button" class="btn btn-primary"
					        onClick={this.save.bind(this)}>Save
					</button>
					<button type="button" class="btn btn-default" onClick={this.cancel.bind(this)}>Cancel</button>
				</div>
		);
	}

	skillChange(skill) {
		this.setState({
			skill: skill
		});
	}

	stateToItem() {
		let {_id, name, description, skill, startingPowerPoints, startingPowers} = this.state;
		return {_id, name, description, skill, startingPowerPoints, startingPowers};
	}

}

export default ArcaneEditor;