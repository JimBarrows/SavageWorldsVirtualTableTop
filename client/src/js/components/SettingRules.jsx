import React from "react";
import SettingRule from "./SettingRule";

class SettingRules extends React.Component {

	add() {
		this.setState({
			adding: true
		})
	}

	addSettingRule(newRule) {
		let rules = [...this.state.rules, newRule];
		this.setState({
			rules,
			adding: false
		});
		this.props.settingRulesChange(rules);
	}

	componentWillMount() {
		let {rules = []}  = this.props;
		this.setState({
			rules,
			adding: false
		});
	}

	componentWillReceiveProps(nextProps) {
		let {rules = []}  = nextProps;
		this.setState({
			rules
		});
	}

	removeRule(rule) {
		let newRules = this.state.rules.filter((r) => rule._id ? rule._id !== r._id : rule.name !== r.name);
		this.setState({
			rules: newRules
		});
		this.props.settingRulesChange(newRules);
	}

	render() {
		let {rules, adding}       = this.state;
		let rulesElements         = rules.map((rule, index) => <SettingRule
				key={rule._id || `${rule.name.replace(" ", "_")}_${index}`}
				rule={rule} index={index}
				save={this.addSettingRule.bind(this)}
				update={this.updateSettingRule.bind(this)}
				remove={this.removeRule.bind(this)}/>
		);
		let addButton             = adding ?
				<SettingRule adding={true} save={this.addSettingRule.bind(this)}/>
				:
				<button type="button" class="btn btn-default btn-xs" onClick={this.add.bind(this)}>
					<span
							class="glyphicon glyphicon-plus"
							aria-hidden="true"/>Add Rule
				</button>;
		return (
				<div id="settingRules">
					<h2>Setting Rules</h2>
					{addButton}
					<dl>
						{rulesElements}
					</dl>
				</div>
		);
	}

	updateSettingRule(settingRule) {
		let index               = this.state.rules.findIndex((rule) => rule._id === settingRule._id || rule.name === settingRule.name);
		this.state.rules[index] = settingRule;
		this.setState({
			rules: this.state.rules
		});
		this.props.settingRulesChange(this.state.rules);
	}
}

export default SettingRules;

