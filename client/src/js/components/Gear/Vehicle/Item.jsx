import React from "react";
import {EditableCell, RowControlButtons} from "bootstrap-react-components";

class Vehicle extends React.Component {

	accelerationChange(e) {
		this.state.item.acceleration = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	armorPointsChange(e) {
		this.state.item.armorPoints = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	climbChange(e) {
		this.state.item.climb = e.target.value;
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

	costChange(e) {
		this.state.item.cost = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	crewChange(e) {
		this.state.item.crew = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	eraChange(e) {
		this.state.item.era = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	edit() {
		this.setState({
			editing: true
		});
	}

	examplesChange(e) {
		this.state.item.examples = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	modeChange(e) {
		this.state.item.mode = e.target.value;
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

	notesChange(e) {
		this.state.item.notes = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	passengersChange(e) {
		this.state.item.passengers = e.target.value;
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

	remove() {
		this.props.remove(this.state.item);
	}

	render() {
		let {allowEditing, editing, item} = this.state;
		return <tr key={item._id}>
			<EditableCell value={item.era}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.eraChange.bind(this)}
			              required={true}
			              type="text"/>
			<EditableCell value={item.type}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.typeChange.bind(this)}
			              required={true}
			              type="text"/>
			<EditableCell value={item.mode}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.modeChange.bind(this)}
			              required={true}
			              type="text"/>
			<EditableCell value={item.name}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.nameChange.bind(this)}
			              required={true}
			              type="text"/>
			<EditableCell value={item.acceleration}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.accelerationChange.bind(this)}
			              required={true}
			              type="number"/>
			<EditableCell value={item.topSpeed}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.topSpeedChange.bind(this)}
			              required={true}
			              type="number"/>
			<EditableCell value={item.toughness}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.toughnessChange.bind(this)}
			              required={true}
			              type="number"/>
			<EditableCell value={item.armorPoints}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.armorPointsChange.bind(this)}
			              required={true}
			              type="number"/>
			<EditableCell value={item.crew}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.crewChange.bind(this)}
			              required={true}
			              type="number"/>
			<EditableCell value={item.passengers}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.passengersChange.bind(this)}
			              required={true}
			              type="number"/>
			<EditableCell value={item.cost}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.costChange.bind(this)}
			              required={true}
			              type="number"/>
			<EditableCell value={item.notes}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.notesChange.bind(this)}
			              required={true}
			              type="text"/>
			<EditableCell value={item.examples}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.examplesChange.bind(this)}
			              required={true}
			              type="text"/>
			<EditableCell value={item.weapons}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.weaponsChange.bind(this)}
			              required={true}
			              type="text"/>
			<EditableCell value={item.climb}
			              disabled={false}
			              edit={allowEditing && editing}
			              onChange={this.climbChange.bind(this)}
			              required={true}
			              type="number"/>

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

	topSpeedChange(e) {
		this.state.item.topSpeed = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	toughnessChange(e) {
		this.state.item.toughness = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	typeChange(e) {
		this.state.item.type = e.target.value;
		this.setState({
			item: this.state.item
		});
	}

	weaponsChange(e) {
		this.state.item.weapons = e.target.value;
		this.setState({
			item: this.state.item
		});
	}
}

export default Vehicle;