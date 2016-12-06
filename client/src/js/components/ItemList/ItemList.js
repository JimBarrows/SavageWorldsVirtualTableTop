import React from "react";
import ObjectId from "bson-objectid";

class ListManager extends React.Component {

	add() {
		this.setState({
			adding: true
		})
	}

	addToList(item) {
		if (!item._id) {
			item._id = ObjectId.generate();
		}
		let list = [...this.state.list, item];
		this.setState({
			list,
			adding: false
		});
		this.props.onListChange(list);
	}

	buttonEditOrNothing(buttonName, editor) {
		let {allowEditing, adding}       = this.state;
		if (allowEditing) {
			if (adding) {
				return editor

			} else {
				return <button type="button" class="btn btn-default btn-xs" onClick={this.add.bind(this)}>
					<span
							class="glyphicon glyphicon-plus"
							aria-hidden="true"/>{buttonName}
				</button>
			}
		} else {
			return "";
		}
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
			adding: false,
			allowEditing: false,
		}
	}

	propsToState(props) {
		let {list, adding, allowEditing}  = props;
		this.setState({
			list,
			adding,
			allowEditing
		});
	}

	removeItem(item) {
		let list = this.state.list.filter((r) => item._id !== r._id);
		this.setState({
			list
		});
		this.props.onListChange(list);
	}


	updateItem(settingRule) {
		let index              = this.state.list.findIndex((rule) => rule._id === settingRule._id || rule.name === settingRule.name);
		this.state.list[index] = settingRule;
		this.setState({
			list: this.state.list
		});
		this.props.onListChange(this.state.list);
	}
}

export default ListManager;