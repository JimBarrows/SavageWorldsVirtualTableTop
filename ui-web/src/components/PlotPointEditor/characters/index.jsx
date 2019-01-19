import PropTypes from 'prop-types'
import React from 'react'

export default class Characters extends React.Component {

  static defaultProps = {}

  static propTypes = {
    id: PropTypes.string.isRequired
  }

  render() {
    let {id} = this.props
    return (
      <div id={'CharacterEditorList-' + id}>
        CharacterEditorList component
      </div>
    )
  }
}

