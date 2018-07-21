import {FormGroup} from 'bootstrap-react-components'
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


  render() {
    let {disabled, id, label, onChange, required, value, valid, validationMessage} = this.props

    return <FormGroup id={'DiceSelectFormGroup-' + id} label={label} required={required} valid={valid}
                      validationMessage={validationMessage}>
      <DiceSelect id={'DiceSelectFormGroup-' + id} onChange={onChange} value={value} required={required}
                  disabled={disabled}/>
    </FormGroup>
  }
}
