import React from "react";
import RaceDescription from "./RaceDescription";
import RaceEditor from "../components/RaceEditor";

class RaceList extends React.Component {

	add() {
		this.setState({
			adding: true
		})
	}

	addToList(newItem) {
		let list = [...this.state.list, newItem];
		this.setState({
			list,
			adding: false
		});
		this.props.onListChange(list);
	}

	componentWillMount() {
		let {list = []}  = this.props;
		this.setState({
			list,
			adding: false
		});
	}

	componentWillReceiveProps(nextProps) {
		let {list = []}  = nextProps;
		this.setState({
			list
		});
	}

	removeItem(item) {
		let list = this.state.list.filter((r) => r._id !== item._id);
		this.setState({
			list
		});
		this.props.onListChange(list);
	}

	render() {
		let {list, adding}       = this.state;
		let listElements         = list.map((item, index) => <RaceDescription
				key={item._id || `${item.name.replace(" ", "_")}_${index}`}
				item={item} index={index}
				save={this.updateItem.bind(this)}
				remove={this.removeItem.bind(this)}/>
		);
		let addButton            = adding ?
				<RaceEditor save={this.addToList.bind(this)}/>
				:
				<button type="button" class="btn btn-default btn-xs" onClick={this.add.bind(this)}>
					<span
							class="glyphicon glyphicon-plus"
							aria-hidden="true"/>Add Race
				</button>;
		return (
				<div id="races">
					<h2>Races</h2>
					{addButton}
					{listElements}
				</div>
		);
	}

	updateItem(item) {
		let index              = this.state.list.findIndex((i) => i._id === item._id);
		this.state.list[index] = item;
		this.setState({
			list: this.state.list
		});
		this.props.onListChange(this.state.list);
	}
}

export default RaceList;