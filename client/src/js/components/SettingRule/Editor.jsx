import {TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import React from "react";
import {ItemEditor} from "../Item";

class SettingRuleEditor extends ItemEditor {

	propsToState(props) {
		let {_id, name, description} = props;
		this.setState({
			_id, name, description
		});
	}

	render() {
		let {_id, name, description} = this.state;
		return (
				<div class="SettingRuleForm">
					<TextFormGroup label="Rule Name"
					               name="name"
					               onChange={this.nameChange.bind(this)}
					               value={name}/>
					<TextAreaFormGroup label="Rule Description"
					                   name="description"
					                   onChange={this.descriptionChange.bind(this)}
					                   value={description}/>
					<button type="button"
					        class="btn btn-default"
					        onClick={this.save.bind(this)}>Save
					</button>
					<button type="button"
					        class="btn btn-default"
					        onClick={this.cancel.bind(this)}>Cancel
					</button>
				</div>
		);
	}

	stateToItem() {
		let {_id, name, description} = this.state;
		return {
			_id, name, description
		}
	}
}

export default SettingRuleEditor;