'use strict';
import React from "react";
import {DangerAlert, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";

class PlotPointForm extends React.Component {

	cancel(event) {
		event.preventDefault();
		this.props.onCancel();
	}

	componentWillMount() {
		let {name = "", description = "", _id = ""}  = this.props;
		this.setState({
			name, description, _id
		});
	}

	componentWillReceiveProps(nextProps) {
		let {name = "", description = "", _id = ""}  = nextProps;
		this.setState({
			name, description, _id
		});
	}

	descriptionChange(event) {
		this.setState({
			description: event.target.value
		})
	}

	nameChange(event) {
		this.setState({
			name: event.target.value
		});
	}

	render() {
		let {name, description, error} = this.state;
		return (
				<form id="plotPointForm">
					<DangerAlert error={error}/>
					<TextFormGroup
							error={error}
							label="Name"
							name="name"
							onChange={this.nameChange.bind(this)}
							value={name}
					/>
					<TextAreaFormGroup label="Description" id="description"
					                   onChange={this.descriptionChange.bind(this)} value={description}/>
					<button type="button" class="btn btn-primary" onClick={this.submit.bind(this)}>Save</button>
					<button type="button" class="btn btn-default" onClick={this.cancel.bind(this)}>Cancel</button>
				</form>
		);
	}

	submit(event) {
		event.preventDefault();
		this.props.onSubmit(this.state);
	}
}
export default PlotPointForm;