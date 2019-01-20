import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../components/EditorList'
import Editor     from './Editor'

export default class Index extends React.Component {

	static propTypes = {
		id                  : PropTypes.string.isRequired,
		specialWeapons      : PropTypes.array.isRequired,
		specialWeaponsChange: PropTypes.func.isRequired

	}

	static defaultProps = {}

	render() {
		return (
			<div id={'SpecialWeaponsEditorListComponent_' + this.props.id}>
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
						burstTemplate  : ' ',
						note           : ' ',
						era            : ' ',
						kind           : ' ',
						military       : false,
						armorPiercing  : 0
					})}
					id={'specialWeaponsEditorList'}
					list={this.props.specialWeapons}
					onChange={this.props.specialWeaponsChange}
					title={'Special Weapons'}>
					<Editor/>
				</EditorList>
			</div>
		)
	}
}

