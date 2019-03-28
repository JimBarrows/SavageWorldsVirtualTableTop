import {Button, SelectFormGroup} from 'bootstrap-react-components'
import PropTypes                 from 'prop-types'
import React                     from 'react'
import SelectedHindranceEditor   from './Editor'

export default class Index extends React.Component {

	static propTypes = {
		id                 : PropTypes.string.isRequired,
		hindrancesAvailable: PropTypes.array.isRequired,
		hindrances         : PropTypes.array.isRequired,
		onChange           : PropTypes.func.isRequired
	}

	static defaultProps = {}

	addHindrance = e => {
		this.props.onChange([...this.props.hindrances, {
			hindrance: this.state.selectedHindrance,
			note     : ''
		}].sort((l, r) => l.hindrance.name < r.hindrance.name ? -1 : l.hindrance.name < r.hindrance.name ? 1 : 0))
		this.setState({selectedHindrance: null})
	}

	newHindranceSelected = e => this.setState({selectedHindrance: this.props.hindrancesAvailable.find(hindrance => hindrance.name === e.target.value)})

	render () {
		const component_class      = `SelectedHindranceList`
		const component_id         = `${component_class}-${this.props.id}`
		const hindrance_names      = this.props.hindrances.map(s => s.hindrance.name)
		const hindrances_remaining = this.props.hindrancesAvailable
			.filter(hindrance => !hindrance_names.includes(hindrance.name))
			.map(hindrance => ({
				label: hindrance.name,
				value: hindrance.name
			}))
		return <div id={component_id} class={component_class} >
			<h3 >Hindrances</h3 >
			<SelectFormGroup id={component_id} label={'Hindrances'} onChange={this.newHindranceSelected}
				options={hindrances_remaining}
				value={this.state.selectedHindrance ? this.state.selectedHindrance.name : ''} />
			<Button id={`${component_id}-AddButton`} onClick={this.addHindrance} >Add</Button >
			{
				this.props.hindrances.map((hindrance, index) => <SelectedHindranceEditor id={component_id} key={index}
					hindrance={hindrance} />)
			}
		</div >
	}

	state = {
		selectedHindrance: null
	}
}
