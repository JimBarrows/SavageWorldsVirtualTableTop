import {TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import React from "react";
import {ItemEditor} from "../../Item";

class TrappingEditor extends ItemEditor {

	propsToState(props) {
		let {_id, name, description, type} = props;
		this.setState({
			_id, name, description, type
		});
	}

	render() {
		let {_id, name, description, type} = this.state;
		return (
				<div id="TrappingEditorForm">
					<TextFormGroup
							label="Name"
							onChange={this.nameChange.bind(this)}
							value={name}
							required={true}/>
					<TextFormGroup
							label="Type"
							onChange={this.typeChange.bind(this)}
							value={type}
							required={true}/>
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

	typeChange(e) {
		this.setState({
			type: e.target.value
		});
	}

	stateToItem() {
		let {_id, name, description, type} = this.state;
		return {_id, name, description, type};
	}

}

export default TrappingEditor;