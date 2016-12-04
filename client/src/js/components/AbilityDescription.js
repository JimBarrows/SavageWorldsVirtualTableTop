import React from "react";
import AbilityEditor from "./AbilityEditor";
import AbilityView from "./AbilityView";

const emptyAbility = {
	name: "",
	description: "",
	_id: "",
	cost: 0
};


class AbilityDescription extends React.Component {

	constructor(props) {
		super(props);
		this.state = {
			editing: false,
			allowEditing: false
		}
	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(nextProps) {
		this.propsToState(nextProps);
	}

	editing(e) {
		this.setState({
			editing: true
		});
	}

	propsToState(props) {
		let {_id, name, description, cost} = props.item;
		let {allowEditing}                 = props;
		this.setState({
			_id, name, description, cost, allowEditing
		});
	}

	remove() {
		let {_id, name, description, cost} = this.state;
		this.props.remove({_id, name, description, cost});
	}

	render() {
		let {_id, name, description, cost, editing, allowEditing} = this.state;
		let element                                               = (editing && allowEditing) ?
				<AbilityEditor _id={_id}
				               name={name}
				               description={description}
				               cost={cost}
				               save={this.save.bind(this)}
				               onListChange={this.props.onListChange}/>
				:
				<AbilityView _id={_id}
				             name={name}
				             description={description}
				             cost={cost}
				             edit={this.editing.bind(this)}
				             remove={this.remove.bind(this)}
				             allowEditing={allowEditing}/>;
		return element;
	}

	save(item) {
		this.setState({
			editing: false
		});
		this.props.save(item);
	}
}

export default AbilityDescription;