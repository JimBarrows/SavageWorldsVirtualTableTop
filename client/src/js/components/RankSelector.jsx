import {SelectFormGroup} from "bootstrap-react-components";
import React from "react";

class RankSelector extends SelectFormGroup {

	list() {
		return [{label: "Novice", value: "Novice"},
			{label: "Seasoned", value: "Seasoned"},
			{label: "Veteran", value: "Veteran"},
			{label: "Heroic", value: "Heroic"},
			{label: "Legendary", value: "Legendary"}];
	}

	render() {
		return (
				<SelectFormGroup disabled={false}
				                 error=""
				                 id={this.props.id + "Rank"}
				                 label="Rank"
				                 onChange={this.props.onChange}
				                 options={this.list()}
				                 required={this.props.required}
				                 value={this.props.value}/>
		);
	}
}

export default RankSelector;