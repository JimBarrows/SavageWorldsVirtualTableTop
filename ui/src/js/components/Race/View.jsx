'use strict';
import React from 'react';
import {RowControlButtons} from 'bootstrap-react-components';
import AbilityList from '../Ability/List';

class RaceView extends React.Component {

	render() {
		const {name, description, abilities, edit, remove, save} = this.props;
		return (
				<div class='raceView'>
					<h2>{name} <RowControlButtons id={name}
					                              editing={false}
					                              edit={edit}
					                              save={save}
					                              remove={remove}/>
					</h2>
					{description}
					<AbilityList list={abilities} allowEditing={false}/>
				</div>
		);
	}
}

export default RaceView