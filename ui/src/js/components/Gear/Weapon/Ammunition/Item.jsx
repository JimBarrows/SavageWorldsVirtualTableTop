import React from "react";
import {EditableCell, RowControlButtons} from "bootstrap-react-components";

class Ammunition extends React.Component {

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

	edit() {
		this.setState({
			editing: true
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
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Name"
			              onChange={this.nameChange.bind(this)}
			              placeHolder="Name" required={true} type="text" value={item.name}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Cost"
			              onChange={this.costChange.bind(this)}
			              placeHolder="Cost" required={true} type="number" value={item.cost}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error=""
			              onChange={this.notesChange.bind(this)}
			              required={true} type="text" value={item.notes}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Weight"
			              onChange={this.weightChange.bind(this)}
			              placeHolder="Weight" required={true} type="number" value={item.weight}/>
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

	weightChange(e) {
		this.state.item.weight = e.target.value;
		this.setState({
			item: this.state.item
		});
	}
}

export default Ammunition;