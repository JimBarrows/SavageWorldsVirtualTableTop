'use strict';
import {AddButton} from "bootstrap-react-components";
import React from "react";
import RaceDescription from "./RaceDescription";
import * as Action from "../actions/PlotPointActions";
import {PlotPointEvent} from "../constants";
import PlotPointStore from "../stores/PlotPointStore";
import RaceEditor from "../components/RaceEditor";


export default class RaceList extends React.Component {

	constructor(props) {
		super();
		this.state           = {
			races: props.races,
			showAdd: false
		};
		this.plotPointChange = this.plotPointChange.bind(this);
	}

	componentWillMount() {
		PlotPointStore.on(PlotPointEvent.UPDATE_SUCCESS, this.plotPointChange);
	}

	componentWillUnmount() {
		PlotPointStore.removeListener(PlotPointEvent.UPDATE_SUCCESS, this.plotPointChange);
	}

	plotPointChange() {
		this.setState({
			showAdd: false
		});
	}

	showAddForm() {
		this.setState({
			showAdd: true
		})
	}


	updateRace(race) {
		Action.updateRace(race);
	}

	removeRace(race) {
		Action.removeRace(race);
	}

	render() {
		const {races, showAdd} = this.state;
		let addForm            = null;
		if (showAdd) {
			addForm = (<RaceEditor/>);
		}
		return (
				<div id="raceList">
					<h1>Races<AddButton onClick={this.showAddForm.bind(this)}/></h1>
					{addForm}
					{races.map((race, index) => (
							<RaceDescription abilities={race.abilities}
							                 description={race.description}
							                 _id={race._id}
							                 key={index}
							                 name={race.name}
							                 remove={this.removeRace.bind(this)}/>))}
				</div>
		);
	}
}