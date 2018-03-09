import {SelectFormGroup} from "bootstrap-react-components";
import React from "react";

class DiceSelect extends React.Component {

	diceList() {
		return [{label: "d4", value: "d4"},
			{label: "d6", value: "d6"},
			{label: "d8", value: "d8"},
			{label: "d10", value: "d10"},
			{label: "d12", value: "d12"}];
	}

	render() {
		return (
				<SelectFormGroup disabled={false}
				                 error=""
				                 id={this.props.id + "Dice"}
				                 label="Dice"
				                 onChange={this.props.onChange}
				                 options={this.diceList()}
				                 required={this.props.required}
				                 value={this.props.value}/>
		);
	}
}

export default DiceSelect;