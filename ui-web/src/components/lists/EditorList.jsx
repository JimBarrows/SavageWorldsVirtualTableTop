import PropTypes from 'prop-types'
import React from 'react'

export default class EditorList extends React.Component {

	static propTypes = {
		id       : PropTypes.string.isRequired,
		emptyItem: PropTypes.object.isRequired,
		list     : PropTypes.array.isRequired,
		onChange : PropTypes.func.isRequired,
		title    : PropTypes.string.isRequired
	};

	static defaultProps = {
		headingLevel: 2
	};

	add = event => {
		event.preventDefault();
		this.props.onChange([Object.assign({}, this.props.emptyItem), ...this.props.list]);
	};

	change       = (item, index) => this.props.onChange(this.props.list.map((r, i) => i === index ? item : r));
	delete       = (index) => this.props.onChange(this.props.list.filter((r, i) => i !== index));
	heading      = () => {
		if (this.props.headingLevel === 1) {
			return <h1>{this.props.title}</h1>;
		} else if (this.props.headingLevel === 2) {
			return <h2>{this.props.title}</h2>;
		} else if (this.props.headingLevel === 3) {
			return <h3>{this.props.title}</h3>;
		}
	};
	listElements = () => {
		if (this.props.list.length === 0) {
			return <p>Nothing here</p>;
		} else {
			return this.props.list.map((item, index) =>
					React.cloneElement(this.props.children, {
						id      : this.props.id,
						index,
						item,
						key     : index,
						onChange: this.change,
						onDelete: this.delete
					}));
		}
	};

	render() {
		return (
				<div id={this.props.id}>
					{this.heading()}
					<button id={`add${this.props.title}Button`} className="btn btn-default" onClick={this.add}>Add</button>
					{this.listElements()}
				</div>
		);
	}
}
