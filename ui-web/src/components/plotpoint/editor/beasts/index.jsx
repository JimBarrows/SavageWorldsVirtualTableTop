import PropTypes  from 'prop-types'
import React      from 'react'
import Beast      from '../../../../models/Beast'
import EditorList from '../../../EditorList'
import Editor     from './Editor'

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
					emptyItem={new Beast()}
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

