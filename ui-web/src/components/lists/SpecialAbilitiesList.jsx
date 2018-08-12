import {Button, ListGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'

export default class SpecialAbilitiesList extends React.Component {

	static defaultProps = {
		abilities: []
	}

	static propTypes = {
		// onAdd    : PropTypes.func.isRequired,
		abilities: PropTypes.arrayOf(PropTypes.shape({
			name       : PropTypes.string.isRequired,
			description: PropTypes.string.isRequired
		})),
		id       : PropTypes.string.isRequired
	}

	addSpecialAbility = (e) => {
		e.preventDefault()
		this.props.onChange([{name: ' ', description: ' '}, ...this.props.abilities])
	}

	descriptionChange = indexOfChange => e => this.props.onChange(this.props.abilities.map((ability, index) =>
		indexOfChange === index ? Object.assign({}, ability, {description: e.target.value}) : ability))

	nameChange = indexOfChange => e => this.props.onChange(this.props.abilities.map((ability, index) =>
		indexOfChange === index ? Object.assign({}, ability, {name: e.target.value}) : ability))

	render() {
		let {abilities, id} = this.props
		let component_id    = `SpecialAbilitiesList=${id}`
		return <div id={component_id}>
			<h3>Special Abilities</h3>
			<Button id={component_id} onClick={this.addSpecialAbility}>Add</Button>
			<ListGroup id={component_id} context={'light'}>
				{abilities.map((a, i) => <div key={i}>
					<TextFormGroup id={component_id + `-Name-${i}`} label={'Name'} onChange={this.nameChange(i)}
					               required={true} value={a.name}/>
					<TextAreaFormGroup id={component_id + `-Description-${i}`} label={'Description'}
					                   onChange={this.descriptionChange(i)} value={a.description}/>
				</div>)}
			</ListGroup>

		</div>
	}
}

