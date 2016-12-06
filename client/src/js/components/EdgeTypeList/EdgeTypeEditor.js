import {TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import React from "react";

class EdgeTypeEditor extends React.Component {

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
		let {_id, name, description} = props;
		this.setState({
			_id, name, description
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

	save(event) {
		event.preventDefault();
		let {_id, name, description} = this.state;
		this.props.save({
			_id, name, description
		});

	}
}

export default EdgeTypeEditor;