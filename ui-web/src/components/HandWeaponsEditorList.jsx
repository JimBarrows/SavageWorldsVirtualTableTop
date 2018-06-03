import PropTypes from 'prop-types';
import React from 'react';
import EditorList from './EditorList';
import HandWeaponEditor from './HandWeaponEditor';

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
								notes      : ' ',
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

