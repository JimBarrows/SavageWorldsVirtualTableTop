import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../../EditorList'
import Editor     from './Editor'

export default class Races extends React.Component {

	static propTypes = {
		id         : PropTypes.string.isRequired,
		races      : PropTypes.array.isRequired,
		racesChange: PropTypes.func.isRequired
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
						<Editor/>
					</EditorList>
				</div>
		);
	}
}
