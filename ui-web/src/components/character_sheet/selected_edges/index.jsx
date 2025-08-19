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

	sortEdgesByName = (edges) => {
		return edges.sort((left, right) => {
			if (left.edge.name < right.edge.name) return -1;
			if (left.edge.name > right.edge.name) return 1;
			return 0;
		});
	}

	addEdge = () => {
		if (!this.state.selectedEdge) return;
		
		const edges = this.props.edges || [];
		const newEdges = [...edges, {
			edge: this.state.selectedEdge,
			note: ''
		}];
		
		this.props.onChange(this.sortEdgesByName(newEdges));
		this.setState({selectedEdge: null});
	}

	newEdgeSelected = e => this.setState({selectedEdge: this.props.edgesAvailable.find(edge => edge.name === e.target.value)})

	edgeNoteChange = (edgeIndex) => (e) => {
		const edges = this.props.edges || [];
		const updatedEdges = [...edges];
		updatedEdges[edgeIndex] = {
			...updatedEdges[edgeIndex],
			note: e.target.value
		};
		this.props.onChange(updatedEdges);
	}


	render () {
		const component_class = `SelectedEdgeList`
		const component_id    = `${component_class}-${this.props.id}`
		const edges           = this.props.edges || []
		const edge_names      = edges.map(s => s.edge?.name).filter(Boolean)
		const edges_remaining = this.props.edgesAvailable
			.filter(edge => !edge_names.includes(edge.name))
			.map(edge => ({
				label: edge.name,
				value: edge.name
			}))
		return <div id={component_id} className={component_class} >
			<h3 >Edges</h3 >
			<SelectFormGroup id={component_id} label={'Edges'} onChange={this.newEdgeSelected} options={edges_remaining}
				value={this.state.selectedEdge ? this.state.selectedEdge.name : ''} />
			<Button id={`${component_id}-AddButton`} onClick={this.addEdge} >Add</Button >
			{
				edges.filter(edge => edge.edge).map((edge, index) =>
															 <div key={index} className="selectedEdge" >
																 <h4 >{edge.edge.name}</h4 >
																 {edge.edge.description}
																 <TextAreaFormGroup id={`${component_id}-Note-${index}`} label={'Note'}
																	 onChange={this.edgeNoteChange(index)}
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
