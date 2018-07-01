import {FormControl, FormGroup, InputGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import DiceSelect from '../DiceSelect'

export default class AttributeFormGroup extends React.Component {

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

  static defaultProps = {
    value: {
      dice : '',
      bonus: 0
    }
  }

  diceChange = e => this.props.onChange(Object.assign({}, this.props.value, {dice: e.target.value}))

  bonusChange = e => this.props.onChange(Object.assign({}, this.props.value, {bonus: parseInt(e.target.value, 10)}))

  render() {
    let {disabled, id, label, onChange, required, value, valid, validationMessage} = this.props
    let className                                                                  = 'form-control'
    if (validationMessage) {
      className += valid ? ' is-valid' : ' is-invalid'
    }
    let componentId    = 'AttributeFormGroup-'
    let bonusComponent = ''
    if ((value.bonus && value.bonus > 0) || (value.dice === 'd12')) {
      bonusComponent = <FormControl id={componentId + 'Bonus-' + id} className={className} disabled={disabled} id={id}
                                    onChange={this.bonusChange} type='number' value={value.bonus}/>
    }
    return <FormGroup id={componentId + id} label={label} required={required} valid={valid}
                      validationMessage={validationMessage}>
      <InputGroup id={componentId + 'InputGroup-' + id}>
        <DiceSelect className={className} disabled={disabled} id={id} onChange={this.diceChange}
                    required={required} value={value.dice}/>
        {bonusComponent}

      </InputGroup>
    </FormGroup>
  }
}

