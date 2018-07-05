import {FormControl} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import DiceSelect from './DiceSelect'

export default class AttributeComponent extends React.Component {

  static defaultProps = {
    value: {
      dice : '',
      bonus: 0
    }
  }

  static propTypes = {
    disabled: PropTypes.bool,
    id      : PropTypes.string.isRequired,
    onChange: PropTypes.func.isRequired,
    required: PropTypes.bool,
    value   : PropTypes.shape({
      dice : PropTypes.oneOf(['d4', 'd6', 'd8', 'd10', 'd12']),
      bonus: PropTypes.number
    })
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
    let {append, className, disabled, id, prepend, required, value} = this.props
    let bonusComponent                                              = ''

    if (value.dice === 'd12') {
      bonusComponent =
        <FormControl id={'AttributeComponentBonus-' + id} className={className} disabled={disabled} id={id}
                     onChange={this.bonusChange} type='number' value={value.bonus}/>
    }

    return (
      <div id={'AttributeComponent-' + id} className={'input-group mb-3'}>
        {prepend}
        <DiceSelect className={className} disabled={disabled} id={id} onChange={this.diceChange}
                    required={required} value={value.dice}/>
        {bonusComponent}
        {this.props.children}
        {append}
      </div>
    )
  }
}

