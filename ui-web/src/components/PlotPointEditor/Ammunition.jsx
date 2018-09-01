import PropTypes from 'prop-types'
import React from 'react'
import EditorList from '../EditorList'
import AmmunitionEditor from '../editors/AmmunitionEditor'

export default class AmmunitionEditorList extends React.Component {

	static propTypes = {
		id              : PropTypes.string.isRequired,
		ammunition      : PropTypes.array.isRequired,
		ammunitionChange: PropTypes.func.isRequired
	}

	static defaultProps = {}

	render() {
		return (
			<div id={'AmmunitionEditorListComponent_' + this.props.id}>
				<EditorList
					emptyItem={({
						name  : ' ',
						cost  : '1/2',
						weight: '1/5',
						note  : ' ',
						era   : ' ',
						kind  : ' '
					})}
					id={'ammunitionEditorList'}
					list={this.props.ammunition}
					onChange={this.props.ammunitionChange}
					title={'Ammunition'}>
					<AmmunitionEditor/>
				</EditorList>
			</div>
		)
	}
}
