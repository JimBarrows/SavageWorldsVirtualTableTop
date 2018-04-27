import React from 'react';

class RaceView extends React.Component {

	render() {
		const {name, description} = this.props.race;
		return (
				<div class='raceView'>
					<h2>{name}</h2>
					{description}
				</div>
		);
	}
}

export default RaceView