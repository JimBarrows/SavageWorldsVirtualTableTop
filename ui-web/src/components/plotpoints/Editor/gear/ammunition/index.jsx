import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../components/EditorList'
import Editor     from './Editor'

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
						name       : ' ',
						description: ' ',
						cost       : 1,
						weight     : 1,
						note       : ' ',
						era        : ' ',
						kind       : ' '
					})}
					id={'ammunitionEditorList'}
					list={this.props.ammunition}
					onChange={this.props.ammunitionChange}
					title={'Ammunition'}>
					<Editor/>
				</EditorList>
			</div>
		)
	}
}
