import {Button}  from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React     from 'react'
import Editor    from './Editor'

export default class Index extends React.Component {

	static propTypes = {
		id             : PropTypes.string.isRequired,
		edgesAvailable: PropTypes.array.isRequired,
		edges         : PropTypes.array.isRequired,
		onChange       : PropTypes.func.isRequired
	}

	static defaultProps = {}

	addEdge = (e) => {
		e.preventDefault()
		this.props.onChange([{name: ' ', rank: {dice: 'd4', bonus: 0}, note: ' '}, ...this.props.edges])
	}

	edgeChanged = (indexOfChange, changedEdge) => {
		this.props.onChange(this.props.edges.map((edge, index) => indexOfChange === index ? changedEdge : edge))
	}

	edgeDeleted = indexOfEdge => {
		this.props.edges.splice(indexOfEdge, 1)
		this.props.onChange(this.props.edges)
	}

	edgeEditorList = () => this.props.edges.map((selectedEdge, index) => <Editor key={index}
	                                                                                id={'edge_' + index}
	                                                                                index={index}
	                                                                                onChange={this.edgeChanged}
	                                                                                onDelete={this.edgeDeleted}
	                                                                                edge={selectedEdge}
	                                                                                edgesAvailable={this.props.edgesAvailable}/>)

	render() {
		let componentId = `SelectedEdgeList-${this.props.id}`

		return <div id={componentId}>
			<h3>Edges</h3>
			<Button id={`${componentId}-AddButton`} onClick={this.addEdge}>Add</Button>
			{this.edgeEditorList()}
		</div>
	}
}
