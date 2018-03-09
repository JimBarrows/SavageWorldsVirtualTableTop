import React from "react";
import {NumberFormGroup} from "bootstrap-react-components";
import AttributeSelect from "../../AttributeSelect";
import DiceSelect from "../../DiceSelect";

class DamageCell extends React.Component {

	attributeChange(e) {
		this.state.damage.attribute = e.target.value;
		this.setState({
			damage: this.state.damage
		});
		this.props.onChange(this.state.damage);
	}

	bonusChange(e) {
		this.state.damage.bonus = e.target.value;
		this.setState({
			damage: this.state.damage
		});
		this.props.onChange(this.state.damage);
	}

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

	diceCountChange(e) {
		this.state.damage.diceCount = e.target.value;
		this.setState({
			damage: this.state.damage
		});
		this.props.onChange(this.state.damage);
	}

	diceChange(e) {
		this.state.damage.dice = e.target.value;
		this.setState({
			damage: this.state.damage
		});
		this.props.onChange(this.state.damage);
	}

	propsToState(props) {
		this.setState({
			damage: props.damage,
			edit: props.edit
		});
	}

	render() {
		let {id}                                = this.props;
		let {attribute, bonus, diceCount, dice} = this.state.damage;
		if (this.state.edit) {
			return (<td id={id + "Cell"}>
				<AttributeSelect id={id} value={attribute} onChange={this.attributeChange.bind(this)}/>
				<NumberFormGroup disabled={false} error="" id={id + "DiceCount"} label="No. dice"
				                 onChange={this.diceCountChange.bind(this)} value={diceCount}/>
				<DiceSelect id={id} value={dice} onChange={this.diceChange.bind(this)}/>
				<NumberFormGroup disabled={false} error="" id={id + "Bonus"} label="Bonus"
				                 onChange={this.bonusChange.bind(this)} value={bonus}/>
			</td>);
		} else {
			return (<td id={id + "Value"}>{`${attribute} + ${diceCount}${dice} + ${bonus}`}</td>)
		}

	}
}

export default DamageCell;