import {NumberFormGroup} from 'bootstrap-react-components'
import React from 'react'
import BaseVehicleEditor from './BaseVehicleEditor'

export default class AircraftEditor extends BaseVehicleEditor {

	onClimbChange = e => this.props.onChange(Object.assign({}, this.props.item, {climb: parseInt(e.target.value, 10)}), this.props.index)

	additionalFields = () => <NumberFormGroup id={'climb'} label='Climb' onChange={this.onClimbChange} required={true}
	                                          value={this.props.item.climb}/>
}

