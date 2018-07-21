import {FormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import AttributeComponent from '../AttributeComponent'

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


  render() {
    let {disabled, id, label, onChange, required, value, valid, validationMessage} = this.props
    let className                                                                  = 'form-control'
    if (validationMessage) {
      className += valid ? ' is-valid' : ' is-invalid'
    }
    let componentId    = 'AttributeFormGroup-'

    return <FormGroup id={componentId + id} label={label} required={required} valid={valid}
                      validationMessage={validationMessage}>
      <AttributeComponent id={componentId + id} onChange={onChange} value={value}/>
    </FormGroup>
  }
}

