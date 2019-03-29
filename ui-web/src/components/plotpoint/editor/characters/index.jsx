import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../../EditorList'
import Editor     from './Editor'


export default class Characters extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id              : PropTypes.string.isRequired,
		characters      : PropTypes.array.isRequired,
		charactersChange: PropTypes.func.isRequired,
		skills          : PropTypes.array.isRequired
	}

	render () {
		let component_id = `Characters-${this.props.id}`
		return (
			<div id={component_id} >
				<EditorList
					emptyItem={new Character()}
					id={component_id}
					list={this.props.characters}
					onChange={this.props.charactersChange}
					headingLevel={1}
					title={'Characters'} >
					<Editor id={component_id}
						index={-1}
						item={({})}
						edgesAvailable={this.props.edges}
						hindrancesAvailable={this.props.hindrances}
						skillsAvailable={this.props.skills}
					/>
				</EditorList >
			</div >
		)
	}
}

