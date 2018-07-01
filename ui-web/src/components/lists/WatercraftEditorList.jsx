import PropTypes from 'prop-types'
import React from 'react'
import WatercraftEditor from '../editors/WatercraftEditor'
import EditorList from './EditorList'

export default class WatercraftEditorList extends React.Component {

	static propTypes = {
		id              : PropTypes.string.isRequired,
		watercraft      : PropTypes.array.isRequired,
		watercraftChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'WatercraftEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name        : ' ',
								description : ' ',
								acceleration: 1,
								topSpeed    : 1,
								toughness   : 2,
								armor       : 1,
								minimumCost : 1,
								maximumCost : 2,
								notes       : ' '
							})}
							id={'watercraftEditorList'}
							list={this.props.watercraft}
							onChange={this.props.watercraftChange}
							title={'Watercraft'}>
						<WatercraftEditor/>
					</EditorList>
				</div>
		);
	}
}

