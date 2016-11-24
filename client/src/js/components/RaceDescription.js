'use strict';
import React from "react";
import RaceViewer from "../components/RaceView";
import RaceEditor from "../components/RaceEditor";
import {PlotPointEvent} from "../constants";
import PlotPointStore from "../stores/PlotPointStore";


export default class RaceDescription extends React.Component {

	constructor(props) {
		super(props);
		let {_id, name, description, abilities} = this.props;
		this.state                              = {
			name,
			description,
			abilities,
			edit: false,
			nameError: ""
		};
		this.plotPointChange                    = this.plotPointChange.bind(this);
	}

	componentWillMount() {
		PlotPointStore.on(PlotPointEvent.CURRENT_PLOTPOINT_CHANGE, this.plotPointChange);
	}

	componentWillUnmount() {
		PlotPointStore.removeListener(PlotPointEvent.CURRENT_PLOTPOINT_CHANGE, this.plotPointChange);
	}

	plotPointChange() {
		this.setState({
			edit: false
		});
	}

	edit() {
		this.setState({
			edit: true
		})
	}

	remove() {
		this.props.remove({
			name: this.state.name,
			description: this.state.description,
			abilities: this.state.abilities,
			_id: this.state._id
		})
	}

	render() {
		let element = (this.state.edit) ?
				<RaceEditor name={this.props.name} description={this.props.description} abilities={this.props.abilities}
				            save={this.props.save}/> :
				<RaceViewer name={this.props.name} description={this.props.description} abilities={this.props.abilities}
				            edit={this.edit.bind(this)} remove={this.remove.bind(this)}/>;
		return element
	}
}