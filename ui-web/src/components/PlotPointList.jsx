import PropTypes from 'prop-types';
import React from 'react';
import {Link} from 'react-router-dom';

class PlotPointList extends React.Component {


	render() {
		return (
				<div id={'PlotPointListComponent' }>
					<table className={'table'}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Description</th>
						</tr>
						</thead>
						<tbody>
						{
							this.props.plotPoints.map((pp, index) =>
									<tr id={'plotPoint_' + index} key={index}>
										<td>{pp.name}<Link to={`/plotPointEditor/${pp.name}`}>Edit</Link></td>
										<td>{pp.description}</td>
									</tr>)
						}
						</tbody>
					</table>

				</div>
		);
	}
}

PlotPointList.propTypes = {
	// id: PropTypes.string.isRequired
};

PlotPointList.defaultProps = {
	plotPoints: []
};

export default PlotPointList;