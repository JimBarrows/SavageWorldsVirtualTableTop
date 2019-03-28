import {FontAwesomeIcon}    from '@fortawesome/react-fontawesome'
import {Button, PageHeader} from 'bootstrap-react-components'
import React                from 'react'
import PlotPointList        from '../components/plotpoint/list/index'

export default class PlotPointListPage extends React.Component {

	navigateToNewPlotPoint = () => this.props.history.push('/plot_point/add')

	plotPointList = () => <PlotPointList id={'mainPlotPointList'} plotPoints={this.state.plotPoints} />

	render = () =>
		<div id='PlotPointListPage' >
			<PageHeader id='PlotPointListPage' ><h1 >Plot Points</h1 ></PageHeader >
			<Button id='addPlotPoint' onClick={this.navigateToNewPlotPoint} >
				<FontAwesomeIcon icon={'plus'} />&nbsp;Add
			</Button >
			{this.state.plotPoints.length > 0 ? this.plotPointList() : <p >There are no plot points, please add one</p >}
		</div >

	state = {
		plotPoints: [],
		page      : {},
		links     : {}
	}

	async componentDidMount () {
		let plotPoints = []// await API.get('PlotPointsCRUD', '/PlotPoints')
		if (plotPoints) {
			this.setState({plotPoints})
		}
	}

}

