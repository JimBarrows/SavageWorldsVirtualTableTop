import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import React from "react";

class AbilityEditor extends React.Component {

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
			costError: "",
			descriptionError: ""
		};
	}

	costChange(e) {
		this.setState({
			cost: e.target.value
		});
	}

	descriptionChange(e) {
		this.setState({
			description: e.target.value
		})
	}

	propsToState(props) {
		let {_id, name, description, cost} = props;
		this.setState({
			_id, name, description, cost
		});
	}

	nameChange(e) {
		this.setState({
			name: e.target.value
		})
	}

	render() {
		let {_id, name, description, cost, nameError, costError, descriptionError} = this.state;
		return (
				<div id="AbilityEditorForm">
					<TextFormGroup
							error={nameError}
							label="Ability Name"
							id={_id + "Name"}
							onChange={this.nameChange.bind(this)}
							value={name}/>
					<NumberFormGroup label="Cost" error={costError} id={cost + "Cost"} onChange={this.costChange.bind(this)}
					                 value={cost}/>
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
		let {_id, name, description, cost} = this.state;
		this.props.save({
			_id, name, description, cost
		});

	}
}

export default AbilityEditor;