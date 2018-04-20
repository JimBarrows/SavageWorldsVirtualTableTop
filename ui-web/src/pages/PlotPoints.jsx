import axios from 'axios';
import React from 'react';
import PlotPointList from '../components/PlotPointList';
import {checkHttpStatus, convertErrorToString, parseJSON} from '../utils';

export default class PlotPoints extends React.Component {

	constructor(props) {
		super(props);
		this.state = {plotPoints: []};
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

	render() {
		return (
				<div id='PlotPointsPage'>
					<h1>Plot Points</h1>
					<PlotPointList id={'mainPlotPointList'} plotPoints={this.state.plotPoints}/>
				</div>
		);
	}
}
