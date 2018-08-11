import PropTypes from 'prop-types'
import React from 'react'
import WatercraftEditor from '../editors/WatercraftEditor'
import EditorList from './EditorList'

export default class WatercraftEditorList extends React.Component {

	static propTypes = {
		id              : PropTypes.string.isRequired,
		watercraft      : PropTypes.array.isRequired,
		watercraftChange: PropTypes.func.isRequired
	}

	static defaultProps = {
		item: {
			name        : ' ',
			description : ' ',
			acceleration: 1,
			topSpeed    : 1,
			toughness   : 2,
			armor       : 1,
			minimumCost : 1,
			maximumCost : 2,
			notes       : ' '
		}
	}

	render() {
		const component_id = `WatercraftEditorList-${this.props.id}`
		return (
			<div id={component_id}>
				<EditorList
					emptyItem={this.props.item}
					id={component_id}
					list={this.props.watercraft}
					onChange={this.props.watercraftChange}
					title={'Watercraft'}>
					<WatercraftEditor item={this.props.item} onChange={e => console.log(e)}/>
				</EditorList>
			</div>
		)
	}
}

