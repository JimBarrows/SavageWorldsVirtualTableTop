import PropTypes from 'prop-types'
import React from 'react'
import EdgeEditor from '../editors/EdgeEditor'
import EditorList from '../EditorList'

export default class EdgeEditorList extends React.Component {

	static propTypes = {
		id         : PropTypes.string.isRequired,
		edges      : PropTypes.array.isRequired,
		edgesChange: PropTypes.func.isRequired
	}

	static defaultProps = {}

	render() {
		return (
			<div id={'EdgeEditorListComponent_' + this.props.id}>
				<EditorList emptyItem={({name: ' ', description: ' ', category: ' ', requirements: ' ', effects: ' '})}
				            id={'EdgeEditorList'}
				            list={this.props.edges}
				            onChange={this.props.edgesChange}
				            title={'Edges'}>
					<EdgeEditor/>
				</EditorList>
			</div>
		)
	}
}
