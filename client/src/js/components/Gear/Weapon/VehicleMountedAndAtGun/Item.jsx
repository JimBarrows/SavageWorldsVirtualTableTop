import React from "react";
import {EditableCell, RowControlButtons} from "bootstrap-react-components";
import DamageCell from "../DamageCell";
import RangeCell from "../RangeCell";

class VehicleMountedAndAtGun extends React.Component {

	apRoundDamageChange(damage) {
		this.state.item.apRoundDamage = damage;
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

	edit() {
		this.setState({
			editing: true
		});
	}

	heRoundDamageChange(damage) {
		this.state.item.heRoundDamage = damage;
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
			<RangeCell edit={allowEditing && editing} range={item.range} onChange={this.rangeChange.bind(this)}/>
			<DamageCell disabled={false} edit={allowEditing && editing} error="" id={item._id + "APDamage"}
			            onChange={this.apRoundDamageChange.bind(this)} required={true} damage={item.apRoundDamage}/>
			<DamageCell disabled={false} edit={allowEditing && editing} error="" id={item._id + "heDamage"}
			            onChange={this.heRoundDamageChange.bind(this)} required={true} damage={item.heRoundDamage}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" id={item._id + "ROF"}
			              onChange={this.rateOfFireChange.bind(this)} required={true} type="number" value={item.rateOfFire}/>
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

	typeChange(e) {
		this.state.item.type = e.target.value;
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

export default VehicleMountedAndAtGun;