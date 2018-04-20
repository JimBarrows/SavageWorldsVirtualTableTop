import PropTypes from 'prop-types';
import React from 'react';

class PlotPointList extends React.Component {


	render() {
		return (
				<div id={'PlotPointListComponent_' + this.props.id}>
					<table className={'table'}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Description</th>
						</tr>
						</thead>
						<tbody>
						{this.props.plotPoints.map((pp, index) => <tr id={'plotPoint_' + index} key={index}>
							<td>{pp.name}</td>
							<td>{pp.description}</td>
						</tr>)}
						</tbody>
					</table>

				</div>
		);
	}
}

PlotPointList.propTypes = {
	id: PropTypes.string.isRequired
};

PlotPointList.defaultProps = {
	plotPoints: []
};

export default PlotPointList;