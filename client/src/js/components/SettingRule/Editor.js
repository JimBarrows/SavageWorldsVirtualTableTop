import {TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import React from "react";

class SettingRuleEditor extends React.Component {

	cancel() {

	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(nextProps) {
		this.propsToState(nextProps);
	}

	constructor(props) {
		super(props);
		this.state = {
			nameError: "",
			descriptionError: ""
		};
	}

	descriptionChange(e) {
		this.setState({
			description: e.target.value
		})
	}

	propsToState(props) {
		let {_id, name, description, edgeType} = props;
		this.setState({
			_id, name, description, edgeType
		});
	}

	nameChange(e) {
		this.setState({
			name: e.target.value
		})
	}

	render() {
		let {_id, name, description, nameError, descriptionError} = this.state;
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
				</div>
		);
	}

	save(event) {
		event.preventDefault();
		let {_id, name, description, edgeType} = this.state;
		this.props.save({
			_id, name, description, edgeType
		});

	}
}

export default SettingRuleEditor;