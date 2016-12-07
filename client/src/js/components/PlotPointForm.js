import {DangerAlert, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import React from "react";
import {EdgeList} from "./EdgeList";
import {EdgeTypeList} from "./EdgeType";
import {HindranceList} from "./Hindrance";
import {RaceList} from "./Race";
import {SettingRuleList} from "./SettingRule";
import {SkillList} from "./Skill";


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
			settingRules: [],
			skillDescriptions: [],
			edges: [],
			edgeTypes: [],
			hindrances: []
		}
	}

	descriptionChange(event) {
		this.setState({
			description: event.target.value
		})
	}

	edgesChange(edges) {
		this.setState({
			edges
		});
	}

	edgeTypesChange(edgeTypes) {
		this.setState({
			edgeTypes
		});
	}

	hindrancesChange(hindrances) {
		this.setState({
			hindrances
		});
	}

	nameChange(event) {
		this.setState({
			name: event.target.value
		});
	}

	propsToState(props) {
		let {name = "", description = "", _id = "", settingRules = [], races = [], skillDescriptions = [], edges = [], edgeTypes = [], hindrances = []}  = props.plotPoint;
		this.setState({
			name, description, _id, settingRules, races, skillDescriptions, edges, edgeTypes, hindrances
		});
	}

	racesChange(races) {
		this.setState({
			races
		});
	}

	render() {
		let {name, description, settingRules, races, skillDescriptions, edges, edgeTypes, error, hindrances} = this.state;
		return (
				<form id="plotPointForm">
					<DangerAlert error={error}/>
					<TextFormGroup
							id="plotPointName"
							error={error}
							label="Name"
							name="name"
							onChange={this.nameChange.bind(this)}
							value={name}
					/>
					<TextAreaFormGroup label="Description" id="description"
					                   onChange={this.descriptionChange.bind(this)} value={description}/>
					<SettingRuleList list={settingRules}
					                 onListChange={this.settingRulesChange.bind(this)}
					                 allowEditing={true}/>
					<RaceList list={races}
					          onListChange={this.racesChange.bind(this)}
					          allowEditing={true}/>
					<SkillList list={skillDescriptions} onListChange={this.skillDescriptionsChange.bind(this)}
					           allowEditing={true}/>
					<EdgeTypeList list={edgeTypes} onListChange={this.edgeTypesChange.bind(this)} allowEditing={true}/>
					<EdgeList list={edges} onListChange={this.edgesChange.bind(this)} allowEditing={true}/>
					<HindranceList list={hindrances} onListChange={this.hindrancesChange.bind(this)} allowEditing={true}/>
					<button type="button" class="btn btn-primary" onClick={this.submit.bind(this)}>Save</button>
					<button type="button" class="btn btn-default" onClick={this.cancel.bind(this)}>Cancel</button>
				</form>
		);
	}

	settingRulesChange(newRules) {
		this.setState({
			settingRules: newRules
		});
	}

	skillDescriptionsChange(skillDescriptions) {
		this.setState({
			skillDescriptions
		});
	}

	submit(event) {
		event.preventDefault();
		let {name = "", description = "", _id = "", settingRules = [], races = [], skillDescriptions = [], edges = [], edgeTypes = [], hindrances = []} = this.state;
		this.props.onSubmit({_id, description, name, settingRules, races, skillDescriptions, edges, edgeTypes, hindrances});
	}
}
export default PlotPointForm;