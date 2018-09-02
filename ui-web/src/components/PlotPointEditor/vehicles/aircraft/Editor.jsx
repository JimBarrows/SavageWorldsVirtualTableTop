import {NumberFormGroup} from 'bootstrap-react-components'
import React from 'react'
import VehicleEditor from '../VehicleEditor'

export default class Editor extends VehicleEditor {

	onClimbChange = e => this.props.onChange(Object.assign({}, this.props.item, {climb: parseInt(e.target.value, 10)}), this.props.index)

	additionalAdditionalFields = () => <NumberFormGroup id={'climb'} label='Climb' onChange={this.onClimbChange}
	                                                    required={true}
	                                                    value={this.props.item.climb}/>
}

