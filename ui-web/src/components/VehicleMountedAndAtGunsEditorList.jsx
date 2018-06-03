import PropTypes from 'prop-types';
import React from 'react';
import EditorList from './EditorList';
import VehicleMountedAndAtGunsEditor from './VehicleMountedAndAtGunsEditor';

export default class VehicleMountedAndAtGunsEditorList extends React.Component {

	static propTypes = {
		id                           : PropTypes.string.isRequired,
		vehicleMountedAndAtGuns      : PropTypes.array.isRequired,
		vehicleMountedAndAtGunsChange: PropTypes
	};

	static defaultProps = {};

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
								apDamage       : ' ',
								apArmorPiercing: 1,
								heDamage       : ' ',
								heBurstTemplate: ' ',
								heArmorPiercing: 1,
								rateOfFire     : 1,
								notes          : ' ',
								era            : ' ',
								kind           : ' '
							})}
							id={'vehicleMountedAndAtGunsEditorList'}
							list={this.props.vehicleMountedAndAtGuns}
							onChange={this.props.vehicleMountedAndAtGunsChange}
							title={'Vehicle Mounted & AT Guns'}>
						<VehicleMountedAndAtGunsEditor/>
					</EditorList>
				</div>
		);
	}
}

