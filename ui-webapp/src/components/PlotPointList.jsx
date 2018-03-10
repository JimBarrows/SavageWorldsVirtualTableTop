import PropTypes from 'prop-types'
import React from 'react'

class PlotPointList extends React.Component {

  render () {
    return (
      <div id={'PlotPointListComponent_' + this.props.id} >
        <h1 >PlotPointList</h1 >
      </div >
    )
  }
}

PlotPointList.propTypes = {
  id: PropTypes.string.required
}

PlotPointList.defaultProps = {}

export default PlotPointList