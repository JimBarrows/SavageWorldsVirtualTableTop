import PropTypes from 'prop-types';
import React from 'react';
import EditorList from './EditorList';
import MundaneItemEditor from './MundaneItemEditor';

export default class MundaneItemEditorList extends React.Component {

	static propTypes = {
		id                : PropTypes.string.isRequired,
		mundaneItems      : PropTypes.array.isRequired,
		mundaneItemsChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'MundageItemEditorListComponent_' + this.props.id}>
					<EditorList emptyItem={({name: ' ', description: ' ', cost: 1, weight: 1})}
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

