import React from "react";
import AbilityDescription from "./AbilityDescription";
import AbilityEditor from "./AbilityEditor";

class AbilityList extends React.Component {

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
		let {list, adding, allowEditing}  = this.props;
		this.setState({
			list,
			adding,
			allowEditing
		});
	}

	componentWillReceiveProps(nextProps) {
		let {list, adding, allowEditing}  = nextProps;
		this.setState({
			list,
			adding,
			allowEditing
		});
	}

	constructor(props) {
		super(props);
		this.state = {
			adding: false,
			allowEditing: false,
		}
	}

	removeItem(item) {
		let list = this.state.list.filter((r) => r._id !== item._id);
		this.setState({
			list
		});
		this.props.onListChange(list);
	}

	render() {
		const {list, allowEditing, adding} = this.state;
		let addButton                      = "";
		if (allowEditing) {
			if (adding) {
				<AbilityEditor save={this.addToList.bind(this)}/>

			} else {
				<button type="button" class="btn btn-default btn-xs" onClick={this.add.bind(this)}>
					<span
							class="glyphicon glyphicon-plus"
							aria-hidden="true"/>Add Ability
				</button>
			}
		} else {
			addButton = "";
		}

		if (list && (list.length > 0)) {
			return (
					<div id="AbilityList">
						<h4>Abilities</h4>
						{addButton}
						{list.map((item, index) => (
								<AbilityDescription key={item._id}
								                    item={item}
								                    save={this.updateItem.bind(this)}
								                    remove={this.removeItem.bind(this)}
								                    allowEditing={allowEditing}/>))}
					</div>
			);
		} else {
			return (<div id="AbilityList"></div>);
		}

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

export default AbilityList;