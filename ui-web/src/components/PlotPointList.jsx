import PropTypes from 'prop-types';
import React from 'react';
import client from '../client';

class PlotPointList extends React.Component {

	constructor(props) {
		super(props);
		this.state = {plotPoints: []};
	}

	componentDidMount() {
		client({method: 'GET', path: '/api/plotPoints'})
				.then(response => {
					this.setState({plotPoints: response.entity._embedded.plotPoints});
				});
	}

	render() {
		return (
				<div id={'PlotPointListComponent_' + this.props.id}>
					<h1>PlotPointList</h1>
				</div>
		);
	}
}

PlotPointList.propTypes = {
	id: PropTypes.string.required
};

PlotPointList.defaultProps = {};

export default PlotPointList;