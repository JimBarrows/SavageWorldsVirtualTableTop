import PropTypes from 'prop-types'
import React from 'react'
import EditorList from '../EditorList'
import MundaneItemEditor from '../editors/MundaneItemEditor'

export default class MundaneItemEditorList extends React.Component {

	static propTypes = {
		id                : PropTypes.string.isRequired,
		mundaneItems      : PropTypes.array.isRequired,
		mundaneItemsChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'MundaneItemEditorListComponent_' + this.props.id}>
					<EditorList emptyItem={({name: ' ', description: ' ', cost: 1, weight: 1, note: ' ', kind: ' ', era: ' '})}
					            id={'MundaneItemEditorList'}
					            list={this.props.mundaneItems}
					            onChange={this.props.mundaneItemsChange}
					            title={'Mundane Items'}>
						<MundaneItemEditor/>
					</EditorList>
				</div>
		);
	}
}
