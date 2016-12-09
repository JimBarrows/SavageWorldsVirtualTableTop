import React from "react";
import {EditableCell, RowControlButtons} from "bootstrap-react-components";

class Armor extends React.Component {

	armorChange(e) {
		this.state.item.armor = e.target.value;
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

	edit() {
		this.setState({
			editing: true
		});
	}

	eraChange(e) {
		this.state.item.era = e.target.value;
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

	remove() {
		this.props.remove(this.state.item);
	}

	render() {
		let {allowEditing, editing, item} = this.state;
		return <tr key={item._id}>
			<EditableCell disabled={false} edit={allowEditing && editing}
			              onChange={this.nameChange.bind(this)} required={true} type="text"
			              value={item.name}/>
			<EditableCell disabled={false} edit={allowEditing && editing}
			              onChange={this.costChange.bind(this)}
			              required={true} type="number"
			              value={item.cost}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Era"
			              onChange={this.eraChange.bind(this)}
			              required={true} type="text"
			              value={item.era}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Armor"
			              onChange={this.armorChange.bind(this)}
			              placeHolder="Armor" required={true} type="number"
			              value={item.armor}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Type"
			              onChange={this.typeChange.bind(this)}
			              placeHolder="Type" required={true} type="text"
			              value={item.type}/>
			<EditableCell disabled={false} edit={allowEditing && editing} error="" label="Weight"
			              onChange={this.weightChange.bind(this)}
			              placeHolder="Weight" required={true} type="number"
			              value={item.weight}/>
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

export default Armor;