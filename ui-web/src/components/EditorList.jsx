import PropTypes from 'prop-types';
import React from 'react';

export default class EditorList extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps = {};

	add = event => {
		event.preventDefault();
		this.props.onChange([Object.assign({}, this.props.emptyItem), ...this.props.list]);
	};

	change = (item, index) => this.props.onChange(this.props.list.map((r, i) => i === index ? item : r));
	delete = (index) => this.props.onChange(this.props.list.filter((r, i) => i !== index));

	listElements = () => {
		if (this.props.list.length === 0) {
			return <p>Nothing here</p>;
		} else {
			return this.props.list.map((item, index) =>
					React.cloneElement(this.props.children, {
						key     : index,
						index,
						item,
						onChange: this.change,
						onDelete: this.delete
					}));
		}
	};

	render() {
		return (
				<div id={this.props.id}>
					<h2>{this.props.title}</h2>
					<button id={`add${this.props.title}Button`} className="btn btn-default" onClick={this.add}>Add</button>
					{this.listElements()}
				</div>
		);
	}
}
