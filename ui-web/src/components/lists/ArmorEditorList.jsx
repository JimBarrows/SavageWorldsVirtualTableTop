import PropTypes from 'prop-types'
import React from 'react'
import ArmorEditor from '../editors/ArmorEditor'
import EditorList from './EditorList'

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
								armor      : ' ',
								note       : ' ',
								era        : ' ',
								kind       : ' '
							})}
							id={'ArmorEditorList'}
							list={this.props.armor}
							onChange={this.props.armorChange}
							title={'Armor'}>
						<ArmorEditor/>
					</EditorList>
				</div>
		);
	}
}

