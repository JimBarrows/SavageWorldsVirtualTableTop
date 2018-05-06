import PropTypes from 'prop-types';
import React from 'react';

export default class List extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'ListComponent_' + this.props.id}>
					<h1>List</h1>
				</div>
		);
	}
}

