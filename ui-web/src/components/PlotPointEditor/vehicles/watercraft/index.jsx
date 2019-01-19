import PropTypes from 'prop-types'
import React from 'react'
import EditorList from '../../components/EditorList'
import Editor from './Editor'

export default class Watercraft extends React.Component {

	static propTypes = {
		id              : PropTypes.string.isRequired,
		watercraft      : PropTypes.array.isRequired,
		watercraftChange: PropTypes.func.isRequired
	}

	static defaultProps = {
		item: {
			acceleration: 1,
			armor       : 0,
			crew        : 1,
			description : ' ',
			era         : ' ',
			kind        : ' ',
			maximumCost : 2,
			minimumCost : 1,
			name        : ' ',
			note        : ' ',
			passengers  : 0,
			topSpeed    : 1,
			toughness   : 2
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
					<Editor item={this.props.item} onChange={e => console.log(e)}/>
				</EditorList>
			</div>
		)
	}
}

