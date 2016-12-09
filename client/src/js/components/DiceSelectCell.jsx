import DiceSelect from "./DiceSelect";
import React from "react";

class DiceSelectCell extends React.Component {

	constructor(props) {
		super(props);
		this.state = {
			edit: props.edit
		}
	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(props) {
		this.propsToState(props);
	};

	diceChange(e) {
		this.setState({
			dice: e.target.value
		});
		this.props.onChange(e.target.value);
	}

	propsToState(props) {
		this.setState({
			dice: props.dice,
			edit: props.edit
		});
	}

	render() {
		let {id}                                = this.props;
		let {dice}                              = this.state;
		if (this.state.edit) {
			return (<td id={id + "Cell"}>
				<DiceSelect id={id} value={dice} onChange={this.diceChange.bind(this)}/>
			</td>);
		} else {
			return (<td id={id + "Value"}>{`${dice}`}</td>)
		}

	}
}

export default DiceSelectCell;