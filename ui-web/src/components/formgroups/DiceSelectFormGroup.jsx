import {FormControl, FormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import DiceSelect from '../DiceSelect'

export default class DiceSelectFormGroup extends React.Component {

	static propTypes = {
		disabled         : PropTypes.bool,
		id               : PropTypes.string.isRequired,
		label            : PropTypes.string,
		onChange         : PropTypes.func.isRequired,
		required         : PropTypes.bool,
		value            : PropTypes.shape({
			dice : PropTypes.oneOf(['d4', 'd6', 'd8', 'd10', 'd12']),
			bonus: PropTypes.number
		}),
		valid            : PropTypes.bool,
		validationMessage: PropTypes.string
	}

	diceChange = e => {
		if (e.target.value === 'd12') {
			this.props.onChange(Object.assign({}, this.props.value, {dice: e.target.value, bonus: 0}))
		} else {
			this.props.onChange(Object.assign({}, this.props.value, {dice: e.target.value, bonus: null}))
		}
	}

	bonusChange = e => this.props.onChange(Object.assign({}, this.props.value, {bonus: parseInt(e.target.value, 10)}))

	render() {
		let {disabled, id, label, required, value, valid, validationMessage} = this.props
		let componentId                                                                = `DiceSelectFormGroup-${id}`
		let bonusComponent                                                             = ''
		if (value.dice === 'd12') {
			bonusComponent =
				<FormControl id={componentId} disabled={disabled}
				             onChange={this.bonusChange} type='number' value={value.bonus}/>
		}
		return <FormGroup id={componentId} className={'mb-3'} label={label} required={required} valid={valid}
		                  validationMessage={validationMessage}>
			<DiceSelect id={componentId} onChange={this.diceChange} value={value} required={required}
			            disabled={disabled}/>
			{bonusComponent}
		</FormGroup>
	}
}
