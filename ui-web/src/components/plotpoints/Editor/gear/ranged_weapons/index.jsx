import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../components/EditorList'
import Editor     from './Editor'

export default class RangedWeaponEditorList extends React.Component {

	static propTypes = {
		id                 : PropTypes.string.isRequired,
		rangedWeapons      : PropTypes.array.isRequired,
		rangedWeaponsChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'RangedWeaponEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name           : ' ',
								description    : ' ',
								cost           : 1,
								weight         : 1,
								shortRange     : 1,
								mediumRange    : 2,
								longRange      : 3,
								damage         : ' ',
								rateOfFire     : 1,
								shots          : 1,
								minimumStrength: ' ',
								note           : ' ',
								era            : ' ',
								kind           : ' '
							})}
							id={'RangedWeaponEditorList'}
							list={this.props.rangedWeapons}
							onChange={this.props.rangedWeaponsChange}
							title={'Ranged Weapons'}>
						<Editor/>
					</EditorList>
				</div>
		);
	}
}
