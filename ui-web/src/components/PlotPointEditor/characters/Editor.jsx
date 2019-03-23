import PropTypes      from 'prop-types'
import React          from 'react'
import CharacterSheet from '../components/character_sheet/index'

export default class Editor extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id             : PropTypes.string.isRequired,
		index          : PropTypes.number.isRequired,
		item           : PropTypes.object.isRequired,
		skillsAvailable: PropTypes.array.isRequired
	}

	onChange = item => this.props.onChange(item, this.props.index)

	render () {
		let component_id    = `CharacterEditor-${this.props.index}-${this.props.id}`
		return (
			<CharacterSheet id={component_id}
				item={this.props.item}
				skillsAvailable={this.props.skillsAvailable}
				edgesAvailable={this.props.edgesAvailable}
				onChange={this.onChange} >
			</CharacterSheet >
		)
	}
}

