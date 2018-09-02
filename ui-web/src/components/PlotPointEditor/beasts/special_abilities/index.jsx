import {Button, ListGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import Editor from './Editor'

export default class Index extends React.Component {

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

	deleteSpecialAbility = specialAbility => {
		let {abilities, onChange} = this.props
		onChange(abilities.filter(sa => sa.name !== specialAbility.name && sa.description !== specialAbility.description))
	}

	descriptionChange = indexOfChange => e => this.props.onChange(this.props.abilities.map((ability, index) =>
		indexOfChange === index ? Object.assign({}, ability, {description: e.target.value}) : ability))

	nameChange = indexOfChange => e => this.props.onChange(this.props.abilities.map((ability, index) =>
		indexOfChange === index ? Object.assign({}, ability, {name: e.target.value}) : ability))

	render() {
		let {abilities, id} = this.props
		let component_id    = `SpecialAbilitiesList-${id}`
		return <div id={component_id}>
			<h3>Special Abilities</h3>
			<Button id={component_id} onClick={this.addSpecialAbility}>Add</Button>
			<ListGroup id={component_id} context={'light'}>
				{abilities.map((a, i) => <Editor key={i} description={a.description}
				                                 descriptionChange={this.descriptionChange(i)}
				                                 id={`${component_id}-${i}`} name={a.name}
				                                 nameChange={this.nameChange(i)}
				                                 onDelete={this.deleteSpecialAbility}/>)}
			</ListGroup>

		</div>
	}
}

