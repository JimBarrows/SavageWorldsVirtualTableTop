import React from "react";
import {TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import {ItemEditor} from "../Item";
import AttributeSelect from "../AttributeSelect";

class Editor extends ItemEditor {

	attributeChange(e) {
		this.setState({
			attribute: e.target.value
		});
	}

	propsToState(props) {
		let {_id, name, description, attribute} = props;
		this.setState({
			_id, name, description, attribute
		});
	}

	render() {
		let {_id, name, description, attribute} = this.state;
		return (
				<div id="SkillEditorPage">
					<TextFormGroup label="Skill Name"
					               name="name"
					               onChange={this.nameChange.bind(this)}
					               value={name}/>
					<TextAreaFormGroup label="Skill Description"
					                   name="description"
					                   onChange={this.descriptionChange.bind(this)}
					                   value={description}/>
					<AttributeSelect onChange={this.attributeChange.bind(this)}
					                 value={attribute}/>
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
		let {_id, name, description, attribute = "Agility"} = this.state;
		return {_id, name, description, attribute};
	}
}

export default Editor;