import {NumberFormGroup} from 'bootstrap-react-components'
import React             from 'react'
import GearEditor        from '../GearEditor'

export default class Editor extends GearEditor {

	static defaultProps = {
		id: 'MundaneItemEditorComponent'
	}

	costChange   = e => this.props.onChange(Object.assign({}, this.props.item, {cost: parseInt(e.target.value, 10) || 0}), this.props.index)
	weightChange = e => this.props.onChange(Object.assign({}, this.props.item, {weight: parseInt(e.target.value, 10) || 0}), this.props.index)

	additionalFields = () => <div>

		<NumberFormGroup id={this.props.id + '-Cost'} label='Cost' onChange={this.costChange} required={true}
		                 value={this.props.item.cost}/>
		<NumberFormGroup id={this.props.id + '-Weight'} label='Weight' onChange={this.weightChange} required={true}
		                 value={this.props.item.weight}/>

	</div>

}

