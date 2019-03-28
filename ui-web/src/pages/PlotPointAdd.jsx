import {PageHeader}   from 'bootstrap-react-components'
import * as PropTypes from 'prop-types'
import React          from 'react'
import PlotPointForm  from '../components/plotpoint/editor'
import PlotPoint      from '../models/PlotPoint'


export default class PlotPointAdd extends React.Component {

	static propTypes    = {
		id: PropTypes.string
	}
	static defaultProps = {
		id: 'PlotPointEditorPage'
	}

	cancel = async () => {

	}

	save = async () => {
	}

	render () {
		return <div id={this.props.id} >
			<PageHeader id={this.props.id} ><h1 >New Plot Point</h1 ></PageHeader >
			<PlotPointForm id={'plotPointForm'}
				onSave={this.save}
				onCancel={this.cancel}
				plotPoint={new PlotPoint()} />
		</div >
	}
}


