import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../../../EditorList'
import Editor     from './Editor'

export default class VehicleMountedAndAtGunsEditorList extends React.Component {

	static propTypes = {
		id                           : PropTypes.string.isRequired,
		vehicleMountedAndAtGuns      : PropTypes.array.isRequired,
		vehicleMountedAndAtGunsChange: PropTypes.func.isRequired
	}

	static defaultProps = {}

	render() {
		return (
			<div id={'vehicleMountedAndAtGunsEditorListComponent_' + this.props.id}>
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
						shots          : 0,
						minimumStrength: ' ',
						note           : ' ',
						era            : ' ',
						kind           : ' ',
						apDamage       : ' ',
						apArmorPiercing: 1,
						heDamage       : ' ',
						heBurstTemplate: ' ',
						heArmorPiercing: 1
					})}
					id={'vehicleMountedAndAtGunsEditorList'}
					list={this.props.vehicleMountedAndAtGuns}
					onChange={this.props.vehicleMountedAndAtGunsChange}
					title={'Vehicle Mounted & AT Guns'}>
					<Editor/>
				</EditorList>
			</div>
		)
	}
}
