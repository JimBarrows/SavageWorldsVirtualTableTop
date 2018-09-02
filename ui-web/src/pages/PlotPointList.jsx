import {API} from 'aws-amplify'
import {PageHeader} from 'bootstrap-react-components'
import React from 'react'
import {withRouter} from 'react-router'
import PlotPointList from '../components/PlotPointList'

class PlotPointListPage extends React.Component {

	state                  = {
		plotPoints: [],
		page      : {},
		links     : {}
	};
	navigateToNewPlotPoint = () => this.props.history.push('/plotPointEditor');
	plotPointList          = () => <PlotPointList id={'mainPlotPointList'} plotPoints={this.state.plotPoints}
	                                              page={this.props.page} links={this.props.links}/>;
	render                 = () =>
			<div id='PlotPointListPage'>
				<PageHeader id='PlotPointListPage'><h1>Plot Points</h1></PageHeader>
				<button className={'btn btn-default'} id='addPlotPointButton' type={'button'}
				        onClick={this.navigateToNewPlotPoint}>Add
				</button>
				{this.state.plotPoints.length > 0 ? this.plotPointList() : <p>There are no plot points, please add one</p>}
			</div>;

	async componentDidMount() {
		let plotPoints = await API.get('PlotPointsCRUD', '/PlotPoints');
		if (plotPoints) {
			this.setState({plotPoints});
		}
	}

}

export default withRouter(PlotPointListPage);
