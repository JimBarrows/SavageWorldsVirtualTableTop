import PropTypes from 'prop-types'
import React from 'react'
import HandWeaponEditor from '../editors/HandWeaponEditor'
import EditorList from '../EditorList'

export default class HandWeaponsEditorList extends React.Component {

	static propTypes = {
		id               : PropTypes.string.isRequired,
		handWeapons      : PropTypes.array.isRequired,
		handWeaponsChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'HandWeaponsEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name       : ' ',
								description: ' ',
								cost       : 1,
								weight     : 1,
								damage     : ' ',
								note       : ' ',
								era        : ' ',
								kind       : ' '
							})}
							id={'HandWeaponsEditorList'}
							list={this.props.handWeapons}
							onChange={this.props.handWeaponsChange}
							title={'Hand Weapons'}>
						<HandWeaponEditor/>
					</EditorList>
				</div>
		);
	}
}
