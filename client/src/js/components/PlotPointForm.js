'use strict';
import React from "react";
import {DangerAlert, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import SettingRules from "./SettingRules";
import RaceList from "./RaceList";

class PlotPointForm extends React.Component {

	cancel(event) {
		event.preventDefault();
		this.props.onCancel();
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
			_id: '',
			description: "",
			name: "",
			settingRules: []
		}
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

	settingRulesChange(newRules) {
		this.setState({
			settingRules: newRules
		});
	}

	racesChange(races) {
		this.setState({
			races
		});
	}

	propsToState(props) {
		let {name = "", description = "", _id = "", settingRules = [], races = []}  = props.plotPoint;
		this.setState({
			name, description, _id, settingRules, races
		});
	}

	render() {
		let {name, description, settingRules, races, error} = this.state;
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
					<SettingRules rules={settingRules} settingRulesChange={this.settingRulesChange.bind(this)}/>
					<RaceList list={races} onListChange={this.racesChange.bind(this)}/>
					<button type="button" class="btn btn-primary" onClick={this.submit.bind(this)}>Save</button>
					<button type="button" class="btn btn-default" onClick={this.cancel.bind(this)}>Cancel</button>
				</form>
		);
	}

	submit(event) {
		event.preventDefault();
		let {name = "", description = "", _id = "", settingRules = [], races = []} = this.state;
		this.props.onSubmit({_id, description, name, settingRules, races});
	}
}
export default PlotPointForm;