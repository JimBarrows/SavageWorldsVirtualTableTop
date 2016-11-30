import {TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import React from "react";

const emptyRule = {
	name: "",
	description: ""
};

class SettingRule extends React.Component {

	componentWillMount() {
		let {rule = emptyRule, index = 0, adding = false}  = this.props;
		this.setState({
			rule, index, adding, editing: false
		});
	}

	componentWillReceiveProps(nextProps) {
		let {rule = emptyRule, index = 0, adding = false}   = nextProps;
		this.setState({
			rule, index, adding
		});
	}

	editing(e) {
		this.setState({
			editing: true
		})
	}

	descriptionChange(e) {
		let {rule}       = this.state;
		rule.description = e.target.value;
		this.setState({
			rule
		});
	}

	nameChange(e) {
		let {rule} = this.state;
		rule.name  = e.target.value;
		this.setState({
			rule
		});
	}

	remove() {
		this.props.remove(this.state.rule);
	}

	render() {
		let {rule, index, adding, editing} = this.state;
		let name                           = rule.name || "NewSettingRule";
		let idKey                          = rule._id || `${name.replace(" ", "_")}_${index}`;
		let buttonName                     = adding ? "Add" : "Save";
		let display                        = adding || editing ?
				<div id={idKey}>
					<TextFormGroup label="Rule Name" name="name" onChange={this.nameChange.bind(this)} value={rule.name}/>
					<TextAreaFormGroup label="Rule Description" name="description" onChange={this.descriptionChange.bind(this)}
					                   value={rule.description}/>
					<button type="button" class="btn btn-default" onClick={this.save.bind(this)}>{buttonName}</button>
				</div>
				: <div id={idKey}>
			<dt>{rule.name}
				<div class="btn-group" role="group" aria-label="...">
					<button type="button" class="btn btn-default btn-xs" onClick={this.editing.bind(this)}><span
							class="glyphicon glyphicon-pencil" aria-hidden="true"/></button>
					<button type="button" class="btn btn-danger btn-xs" onClick={this.remove.bind(this)}><span
							class="glyphicon glyphicon-remove" aria-hidden="true"/></button>
				</div>
			</dt>
			<dd>{rule.description}</dd>
		</div>;
		return display;
	}

	save(e) {
		if (this.state.adding) {
			this.props.save(this.state.rule);
		} else {
			this.props.update(this.state.rule);
			this.setState({
				editing: false
			})
		}

	}
}

export default SettingRule;