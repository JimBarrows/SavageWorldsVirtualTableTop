import {SelectFormGroup} from "bootstrap-react-components";
import React from "react";

class AttributeSelect extends React.Component {

	abilityList() {
		return [{label: "Agility", value: "Agility"},
			{label: "Smarts", value: "Smarts"},
			{label: "Strength", value: "Strength"},
			{label: "Spirit", value: "Spirit"},
			{label: "Vigor", value: "Vigor"}];
	}

	render() {
		return (
				<SelectFormGroup disabled={false}
				                 error=""
				                 id={this.props.id + "Attribute"}
				                 label="Attribute"
				                 onChange={this.props.onChange}
				                 options={this.abilityList()}
				                 required={this.props.required}
				                 value={this.props.value}/>
		);
	}
}

export default AttributeSelect;