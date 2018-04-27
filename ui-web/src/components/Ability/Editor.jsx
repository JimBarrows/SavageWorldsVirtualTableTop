import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import React from "react";
import {ItemEditor} from "../../../../ui/src/js/components/Item/index";

class AbilityEditor extends ItemEditor {

	costChange(e) {
		this.setState({
			cost: e.target.value
		});
	}

	propsToState(props) {
		let {_id, name, description, cost} = props;
		this.setState({
			_id, name, description, cost
		});
	}

	render() {
		let {_id, name, description, cost} = this.state;
		return (
				<div id="AbilityEditorForm">
					<TextFormGroup
							label="Ability Name"
							id={_id + "Name"}
							onChange={this.nameChange.bind(this)}
							value={name}/>
					<NumberFormGroup label="Cost"
					                 id={cost + "Cost"}
					                 onChange={this.costChange.bind(this)}
					                 value={cost}/>
					<TextAreaFormGroup
							label="Description"
							id={_id + "Description"}
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

	stateToItem() {
		let {_id, name, description, cost} = this.state;
		return {_id, name, description, cost};
	}

}

export default AbilityEditor;