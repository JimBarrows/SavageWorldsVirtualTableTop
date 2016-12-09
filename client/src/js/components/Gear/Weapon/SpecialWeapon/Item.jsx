import React from "react";
import {EditableCell, RowControlButtons} from "bootstrap-react-components";
import DamageCell from "../DamageCell";
import DiceSelectCell from "../../../DiceSelectCell";
import RangeCell from "../RangeCell";

class SpecialWeapon extends React.Component {

	apChange(e) {
		this.state.item.armorPiercing = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	burstTemplateChange(e) {
		this.state.item.burstTemplate = e.target.value;
		this.setState({
			item: this.state.item
		});
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
			allowEditing: true,
			editing: false
		}
	}

	damageChange(damage) {
		this.state.item.damage = damage;
		this.setState({
			item: this.state.item
		});
	}

	edit() {
		this.setState({
			editing: true
		});
	}

	minStrChange(e) {
		this.state.item.minStr = e;
		this.setState({
			item: this.state.item
		});
	}

	nameChange(e) {
		this.state.item.name = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	propsToState(props) {
		let {allowEditing, editing, item} = props;
		this.setState({
			allowEditing, editing, item
		});
	}

	rangeChange(e) {
		this.state.item.range = e;
		this.setState({
			item: this.state.item
		});
	}

	rateOfFireChange(e) {
		this.state.item.rateOfFire = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	remove() {
		this.props.remove(this.state.item);
	}

	render() {
		let {allowEditing, editing, item} = this.state;
		return <tr key={item._id}>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Name"
			              onChange={this.nameChange.bind(this)}
			              placeHolder="Name" required={true} type="text" value={item.name}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Weight"
			              onChange={this.weightChange.bind(this)}
			              placeHolder="Weight" required={true} type="number" value={item.weight}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="AP"
			              onChange={this.apChange.bind(this)}
			              placeHolder="AP" required={true} type="number" value={item.armorPiercing}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Burst Template"
			              onChange={this.burstTemplateChange.bind(this)}
			              placeHolder="Burst Template" required={true} type="text" value={item.burstTemplate}/>
			<DamageCell disabled={false} edit={allowEditing && editing} error="" id={item._id + "Damage"}
			            onChange={this.damageChange.bind(this)} required={true} damage={item.damage}/>
			<DiceSelectCell dice={item.minStr} edit={allowEditing && editing} onChange={this.minStrChange.bind(this)}/>
			<RangeCell edit={allowEditing && editing} range={item.range} onChange={this.rangeChange.bind(this)}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" id={item._id + "ROF"}
			              onChange={this.rateOfFireChange.bind(this)} required={true} type="number" value={item.rateOfFire}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" id={item._id + "Shots"}
			              onChange={this.shotsChange.bind(this)} required={true} type="number" value={item.shots}/>
			<td><RowControlButtons editing={editing}
			                       edit={this.edit.bind(this)}
			                       save={this.save.bind(this)}
			                       remove={this.remove.bind(this)}/></td>
		</tr>;
	}

	save() {
		this.props.save(this.state.item);
		this.setState({
			editing: false
		})

	}

	shotsChange(e) {
		this.state.item.shots = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	weightChange(e) {
		this.state.item.weight = e.target.value;
		this.setState({
			item: this.state.item
		});
	}
}

export default SpecialWeapon;