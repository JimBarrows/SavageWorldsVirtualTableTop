import PropTypes from 'prop-types';
import React from 'react';
import EditorList from './EditorList';
import RaceEditor from './RaceEditor';

export default class RaceEditorList extends React.Component {

	static propTypes = {
		id   : PropTypes.string.isRequired,
		races: PropTypes.array.isRequired

	};

	static defaultProps = {};

	render() {
		return (
				<div id={'RaceEditorListComponent_' + this.props.id}>
					<EditorList emptyItem={({name: ' ', description: ' ', abilities: []})}
					            id={'RaceEditorList'}
					            list={this.props.races}
					            onChange={this.props.racesChange}
					            title={'Races'}>
						<RaceEditor/>
					</EditorList>
				</div>
		);
	}
}

