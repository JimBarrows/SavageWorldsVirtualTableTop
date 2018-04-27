import React from 'react';
import PropTypes from 'prop-types';
import {RaceEditor} from './index';

class RaceList extends React.Component {

	static propTypes = {
		list: PropTypes.array.isRequired
	};

	static defaultProps = {
		list: []
	};

	render() {

		return (
				<div id='raceList'>
					<h1>Races</h1>
					{this.props.list.map((race, index) => <RaceEditor key={index} race={race}/>)}
				</div>
		);
	}

}

export default RaceList;