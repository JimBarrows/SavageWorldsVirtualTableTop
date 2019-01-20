import PropTypes from 'prop-types'
import React     from 'react'

export default class Editor extends React.Component {

  static defaultProps = {}

  static propTypes = {
    id: PropTypes.string.isRequired
  }

  render() {
    let {id} = this.props
    return (
      <div id={'CharacterEditor-' + id}>
        CharacterEditor component
      </div>
    )
  }
}

