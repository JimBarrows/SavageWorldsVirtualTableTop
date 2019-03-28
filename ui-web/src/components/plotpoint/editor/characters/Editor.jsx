import PropTypes      from 'prop-types'
import React          from 'react'
import CharacterSheet from '../../../character_sheet'

export default class Editor extends React.Component {

	static defaultProps = {}

	static propTypes = {
		edgesAvailable     : PropTypes.array.isRequired,
		hindrancesAvailable: PropTypes.array.isRequired,
		id                 : PropTypes.string.isRequired,
		index              : PropTypes.number.isRequired,
		item               : PropTypes.object.isRequired,
		skillsAvailable    : PropTypes.array.isRequired
	}

	onChange = item => this.props.onChange(item, this.props.index)

	render () {
		let component_id = `CharacterEditor-${this.props.index}-${this.props.id}`
		return (
			<CharacterSheet
				edgesAvailable={this.props.edgesAvailable}
				hindrancesAvailable={this.props.hindrancesAvailable}
				id={component_id}
				item={this.props.item}
				onChange={this.onChange}
				skillsAvailable={this.props.skillsAvailable} >
			</CharacterSheet >
		)
	}
}

