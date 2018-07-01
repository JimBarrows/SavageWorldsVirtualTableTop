import PropTypes from 'prop-types'
import React from 'react'

export default class DiceSelect extends React.Component {

  static defaultProps = {}

  static propTypes = {
    className: PropTypes.string,
    disabled : PropTypes.bool,
    id       : PropTypes.string.isRequired,
    label    : PropTypes.string,
    onChange : PropTypes.func.isRequired,
    required : PropTypes.bool,
    value    : PropTypes.oneOf(['d4', 'd6', 'd8', 'd10', 'd12']),
  }

  render() {
    let {className, disabled, id, onChange, required, value} = this.props
    return (
      <select className={className} disabled={disabled} id={'DiceSelect-' + id} onChange={onChange}
              required={required} value={value}>
        <option/>
        <option value={'d4'}>d4</option>
        <option value={'d6'}>d6</option>
        <option value={'d8'}>d8</option>
        <option value={'d10'}>d10</option>
        <option value={'d12'}>d12</option>
      </select>
    )
  }
}

