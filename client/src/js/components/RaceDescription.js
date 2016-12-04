'use strict';
import React from "react";
import RaceViewer from "../components/RaceView";
import RaceEditor from "../components/RaceEditor";

const emptyRace = {
	name: "",
	description: "",
	_id: "",
	abilities: []
};

class RaceDescription extends React.Component {

	constructor(props) {
		super(props);
		this.state = {
			editing: false
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
		})
	}

	propsToState(props) {
		let {item                               = emptyRace, index = 0, editing = false}  = props;
		let {_id, name, description, abilities} = item;
		this.setState({
			_id, name, description, abilities, index, editing
		});
	}

	remove() {
		let {_id, name, description, abilities} = this.state;
		this.props.remove({_id, name, description, abilities});
	}

	render() {
		let {_id, name, description, abilities} = this.state;
		let element                             = (this.state.editing) ?
				<RaceEditor _id={_id} name={name} description={description} abilities={abilities}
				            save={this.props.save} onListChange={this.props.onListChange}/> :
				<RaceViewer _id={_id} name={name} description={description} abilities={abilities}
				            edit={this.editing.bind(this)} remove={this.remove.bind(this)}/>;
		return element
	}
}

export default RaceDescription;