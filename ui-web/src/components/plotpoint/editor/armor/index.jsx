import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../../EditorList'
import Editor     from './Editor'

export default class ArmorEditorList extends React.Component {

	static propTypes = {
		id         : PropTypes.string.isRequired,
		armor      : PropTypes.array.isRequired,
		armorChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'ArmorEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name       : ' ',
								description: ' ',
								cost       : 1,
								weight     : 1,
								armor      : 0,
								note       : ' ',
								era        : ' ',
								kind       : ' '
							})}
							id={'ArmorEditorList'}
							list={this.props.armor}
							onChange={this.props.armorChange}
							title={'Armor'}>
						<Editor/>
					</EditorList>
				</div>
		);
	}
}
