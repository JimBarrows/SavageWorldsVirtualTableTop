import PropTypes from 'prop-types'
import React from 'react'
import {application_constants} from '../constants'
import './MessageDisplay.css'

class MessageDisplay extends React.Component {

  render () {
    let {app} = this.props
    let message = ""
    let contextClass = "alert-info"
    switch (app.message.context) {
      case application_constants.MESSAGE_CONTEXT_DANGER:
        contextClass = "alert-danger"
        break
      case application_constants.MESSAGE_CONTEXT_INFO:
        contextClass = "alert-info"
        break
      case application_constants.MESSAGE_CONTEXT_SUCCESS:
        contextClass = "alert-success"
        break
      case application_constants.MESSAGE_CONTEXT_WARNING:
        contextClass = "alert-warning"
        break
      default:
        contextClass = "alert-info"
    }
    if (app.message.show) {
      message = <div className={`alert ${contextClass}`} role="alert" >{app.message.message}</div >
    }
    return (
      <div id={'MessageDisplayComponent_' + this.props.id} className={'row message-display'} >
        <div className={"col-12"} >
          {message}
        </div >
      </div >
    )
  }
}

MessageDisplay.propTypes = {
  id: PropTypes.string.isRequired
}

MessageDisplay.defaultProps = {}


export default MessageDisplay