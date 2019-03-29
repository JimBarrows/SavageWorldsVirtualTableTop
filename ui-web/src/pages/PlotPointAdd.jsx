import {API, graphqlOperation} from 'aws-amplify'
import {PageHeader}            from 'bootstrap-react-components'
import * as PropTypes          from 'prop-types'
import React                   from 'react'
import PlotPointForm           from '../components/plotpoint/editor'
import {createPlotPoint}       from '../graphql/mutations'
import PlotPoint               from '../models/PlotPoint'


export default class PlotPointAdd extends React.Component {

	static propTypes    = {
		id: PropTypes.string
	}
	static defaultProps = {
		id: 'PlotPointEditorPage'
	}

	cancel = async () => {
		this.props.history.push('/')
	}

	save = async plotPoint => {
		try {
			let response = await API.graphql(graphqlOperation(createPlotPoint, plotPoint))
			console.log('response: ', response)
			this.props.history.push('/')
		} catch (err) {
			console.log(err)
		}
	}

	render () {
		return <div id={this.props.id} >
			<PageHeader id={this.props.id} ><h1 >New Plot Point</h1 ></PageHeader >
			<PlotPointForm id={'plotPointAdd'}
				onSave={this.save}
				onCancel={this.cancel}
				plotPoint={new PlotPoint()} />
		</div >
	}
}


