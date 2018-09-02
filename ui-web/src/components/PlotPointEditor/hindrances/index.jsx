import PropTypes from 'prop-types'
import React from 'react'
import EditorList from '../components/EditorList'
import Editor from './Editor'

export default class HindranceEditorList extends React.Component {

	static propTypes = {
		id              : PropTypes.string.isRequired,
		hindrances      : PropTypes.array.isRequired,
		hindrancesChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'HindranceEditorListComponent_' + this.props.id}>
					<EditorList emptyItem={({name: ' ', description: ' ', severity: 'Minor'})}
					            id={'HindrancesEditorList'}
					            list={this.props.hindrances}
					            onChange={this.props.hindrancesChange}
					            title={'Hindrances'}>
						<Editor/>
					</EditorList>
				</div>
		);
	}
}
