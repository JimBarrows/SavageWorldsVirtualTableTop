import PropTypes from 'prop-types'
import React from 'react'
import EditorList from '../components/EditorList'
import Editor from './Editor'

export default class Beasts extends React.Component {

	static propTypes = {
		id          : PropTypes.string.isRequired,
		beasts      : PropTypes.array.isRequired,
		beastsChange: PropTypes.func.isRequired,
		skills      : PropTypes.array.isRequired
	}

	static defaultProps = {}

	render() {
		let component_id = `BeastsEditorListComponent_${this.props.id}`
		return (
			<div id={component_id}>
				<EditorList
					emptyItem={({
						agility           : {dice: 'd4', bonus: 0},
						animalIntelligence: false,
						armor             : 0,
						charisma          : 0,
						description       : ' ',
						name              : ' ',
						pace              : 6,
						skills            : [],
						smarts            : {dice: 'd4', bonus: 0},
						specialAbilities  : [],
						spirit            : {dice: 'd4', bonus: 0},
						strength          : {dice: 'd4', bonus: 0},
						vigor             : {dice: 'd4', bonus: 0}
					})}
					id={component_id}
					list={this.props.beasts}
					onChange={this.props.beastsChange}
					headingLevel={1}
					title={'Beasts'}>
					<Editor id={component_id} skillsAvailable={this.props.skills}/>
				</EditorList>
			</div>
		)
	}
}

