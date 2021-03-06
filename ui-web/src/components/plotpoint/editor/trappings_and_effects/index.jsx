import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../../EditorList'
import Editor     from './Editor'

export default class TrappingsAndEffects extends React.Component {

	static propTypes = {
		id                       : PropTypes.string.isRequired,
		trappingsAndEffects      : PropTypes.array.isRequired,
		trappingsAndEffectsChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'TrappingsAndEffectsEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name       : ' ',
								description: ' ',
								effects    : []
							})}
							id={'trappingsAndEffectsEditorList'}
							list={this.props.trappingsAndEffects}
							onChange={this.props.trappingsAndEffectsChange}
							title={'Trappings & Effects'}>
						<Editor/>
					</EditorList>
				</div>
		);
	}
}

