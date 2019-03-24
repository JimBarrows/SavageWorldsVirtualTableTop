import {Button, SelectFormGroup, TextAreaFormGroup} from 'bootstrap-react-components'
import PropTypes                                    from 'prop-types'
import React                                        from 'react'

export default class Index extends React.Component {

	static propTypes = {
		id            : PropTypes.string.isRequired,
		edgesAvailable: PropTypes.array.isRequired,
		edges         : PropTypes.array.isRequired,
		onChange      : PropTypes.func.isRequired
	}

	static defaultProps = {}

	addEdge = e => {
		this.props.onChange([...this.props.edges, {
			edge: this.state.selectedEdge,
			note: ''
		}].sort((l, r) => l.edge.name < r.edge.name ? -1 : l.edge.name < r.edge.name ? 1 : 0))
		this.setState({selectedEdge: null})
	}

	newEdgeSelected = e => this.setState({selectedEdge: this.props.edgesAvailable.find(edge => edge.name === e.target.value)})

	edgeNoteChange = edge => e => edge.note = e.target.value


	render () {
		const component_class = `SelectedEdgeList`
		const component_id    = `${component_class}-${this.props.id}`
		const edge_names      = this.props.edges.map(s => s.edge.name)
		const edges_remaining = this.props.edgesAvailable
			.filter(edge => !edge_names.includes(edge.name))
			.map(edge => ({
				label: edge.name,
				value: edge.name
			}))
		return <div id={component_id} class={component_class} >
			<h3 >Edges</h3 >
			<SelectFormGroup id={component_id} label={'Edges'} onChange={this.newEdgeSelected} options={edges_remaining}
				value={this.state.selectedEdge ? this.state.selectedEdge.name : ''} />
			<Button id={`${component_id}-AddButton`} onClick={this.addEdge} >Add</Button >
			{
				this.props.edges.map((edge, index) =>
															 <div key={index} class="selectedEdge" >
																 <h4 >{edge.edge.name}</h4 >
																 {edge.edge.description}
																 <TextAreaFormGroup id={`${component_id}-Description`} label={'Description'}
																	 onChange={this.edgeNoteChange(edge)}
																	 value={edge.note} />
															 </div >
				)
			}
		</div >
	}

	state = {
		selectedEdge: null
	}
}
