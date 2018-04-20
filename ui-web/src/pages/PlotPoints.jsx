import axios from 'axios';
import {PageHeader} from 'bootstrap-react-components';
import React from 'react';
import {withRouter} from 'react-router';
import PlotPointList from '../components/PlotPointList';
import {checkHttpStatus, convertErrorToString, parseJSON} from '../utils';

class PlotPoints extends React.Component {

	constructor(props) {
		super(props);
		this.state = {plotPoints: []};
		this.navigateToNewPlotPoint = this.navigateToNewPlotPoint.bind(this);
	}

	componentDidMount() {
		axios.get('/api/plotPoints')
				.then(checkHttpStatus)
				.then(parseJSON)
				.then(data => this.setState({
					plotPoints: data._embedded.plotPoints,
					page      : data.page,
					links     : data._links
				}))
				.catch(error => console.log('error: ', convertErrorToString(error)));
	}

	navigateToNewPlotPoint() {
		this.props.history.push( '/newPlotPoint')
	}
	render() {
		return (
				<div id='PlotPointsPage'>
					<PageHeader id='PlotPointsPage'><h1>Plot Points</h1></PageHeader>
					<button className={'btn btn-default'} id='addPlotPointButton' type={'button'}
					        onClick={this.navigateToNewPlotPoint}>Add</button>
					<PlotPointList id={'mainPlotPointList'} plotPoints={this.state.plotPoints}/>
				</div>
		);
	}
}

export default withRouter(PlotPoints);
