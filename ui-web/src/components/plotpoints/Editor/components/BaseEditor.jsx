import {Panel, PanelBody, PanelHeader, RemoveButton} from 'bootstrap-react-components'
import PropTypes                                     from 'prop-types'
import React                                         from 'react'

export default class BaseEditor extends React.Component {

  static propTypes = {
    id      : PropTypes.string.isRequired,
    onDelete: PropTypes.func.isRequired
  }

  static defaultProps = {}

  render() {
    return <Panel id={this.props.id}>

      <PanelHeader id={this.props.id}>
        <div className={'btn-group float-right'}>
          <RemoveButton id={this.props.id} onClick={this.props.onDelete}/>
        </div>
      </PanelHeader>
      <PanelBody id={this.props.id}>
        {this.props.children}
      </PanelBody>
    </Panel>

  }
}

