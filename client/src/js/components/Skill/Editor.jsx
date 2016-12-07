import React from "react";
import {FormGroup, TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";

class Editor extends React.Component {

	attributeChange(e) {
		this.setState({
			attribute: e.target.value
		});
	}

	cancel() {

	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(nextProps) {
		this.propsToState(nextProps);
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
					<FormGroup label="Attribute"
					           id="attribute"
					           required={true}>
						<select class="form-control"
						        id="attributeSelect"
						        onChange={this.attributeChange.bind(this)}
						        value={attribute}>
							<option>Agility</option>
							<option>Smarts</option>
							<option>Strength</option>
							<option>Spirit</option>
							<option>Vigor</option>
						</select>
					</FormGroup>
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

	save(event) {
		event.preventDefault();
		let {_id, name, description, attribute = "Agility"} = this.state;
		this.props.save({
			_id, name, description, attribute
		});

	}
}

export default Editor;