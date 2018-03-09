import {TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import React from "react";
import {ItemEditor} from "../Item";

class EdgeTypeEditor extends ItemEditor {

	propsToState(props) {
		let {_id, name, description} = props;
		this.setState({
			_id, name, description
		});
	}

	render() {
		let {_id, name, description, nameError, descriptionError} = this.state;
		return (
				<div id="EdgeEditorForm">
					<TextFormGroup
							error={nameError}
							label="Edge Name"
							id={_id + "Name"}
							onChange={this.nameChange.bind(this)}
							value={name}/>
					<TextAreaFormGroup
							error={descriptionError}
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
		let {_id, name, description} = this.state;
		return {
			_id, name, description
		}
	}
}

export default EdgeTypeEditor;