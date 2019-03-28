import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../../EditorList'
import Editor     from './Editor'

export default class ArcaneBackgrounds extends React.Component {

	static propTypes = {
		id                    : PropTypes.string.isRequired,
		arcaneBackgrounds     : PropTypes.array.isRequired,
		arcaneBackgroundChange: PropTypes.func.isRequired
	}

	static defaultProps = {}

	render() {
		return (
			<div id={'ArcaneBackgroundEditorListComponent_' + this.props.id}>
				<EditorList
					emptyItem={({
						name               : ' ',
						description        : ' ',
						skillName          : ' ',
						attribute          : ' ',
						startingPowers     : 2,
						startingPowerPoints: 10
					})}
					id={'arcaneBackgroundEditorList'}
					list={this.props.arcaneBackgrounds}
					onChange={this.props.arcaneBackgroundChange}
					title={'Arcane Backgrounds'}>
					<Editor/>
				</EditorList>
			</div>
		)
	}
}

