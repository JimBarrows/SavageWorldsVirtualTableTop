'use strict';
import React from "react";
import {RowControlButtons} from "bootstrap-react-components";

export default class RaceView extends React.Component {

	constructor(props) {
		super(props);
		this.state = {
			editing: false
		}
	}

	render() {
		const {name, description, abilities} = this.props;
		const {editing}                      = this.state;
		return (
				<div id="raceView">
					<h2>{name} <RowControlButtons id={name} editing={editing} edit={this.props.edit} save={this.props.save}
					                              remove={this.props.remove}/>
					</h2>
					{description}
					<h3>Abilities</h3>
					{abilities.map((ability, index) => (
							<div key={index} class="ability">
								<h4 >{ability.name}
									<small>
										<bold>Cost:</bold>
										{ability.cost}</small>
								</h4>
								{ability.description}
							</div>))}
				</div>
		);
	}
}