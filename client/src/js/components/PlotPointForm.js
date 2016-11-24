'use strict';
import React from "react";
import RaceList from "../components/RaceList";
import {DangerAlert, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import PlotPointStore from "../stores/PlotPointStore";

export default class PlotPointForm extends React.Component {

	cancel(event) {
		event.preventDefault();
		this.props.onCancel();
	}

	constructor(props) {
		super(props);
		let {_id, description, name, races} = PlotPointStore.current;
		this.state                          = {
			_id,
			description,
			error: null,
			name,
			races
		};
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
		let {name, description, races, error} = this.state;
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
					<RaceList races={races}/>
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