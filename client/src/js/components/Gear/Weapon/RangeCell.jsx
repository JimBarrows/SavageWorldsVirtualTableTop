import React from "react";
import {NumberFormGroup} from "bootstrap-react-components";

class RangeCell extends React.Component {

	constructor(props) {
		super(props);
		this.state = {
			edit: props.edit
		}
	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(props) {
		this.propsToState(props);
	};

	longChange(e) {
		let long   = e.target.value;
		let range  = this.state.range;
		range.long = long;
		this.setState({
			range
		});
		this.props.onChange(range);
	}

	mediumChange(e) {
		let medium   = e.target.value;
		let range    = this.state.range;
		range.medium = medium;
		this.setState({
			range
		});
		this.props.onChange(range);
	}

	propsToState(props) {
		this.setState({
			range: props.range,
			edit: props.edit
		});
	}

	render() {
		let {id}                                = this.props;
		let {range}                             = this.state;
		if (this.state.edit) {
			return (<td id={id + "Cell"}>
				<NumberFormGroup label="Short" onChange={this.shortChange.bind(this)} value={range.short}/>
				<NumberFormGroup label="Medium" onChange={this.mediumChange.bind(this)} value={range.medium}/>
				<NumberFormGroup label="Large" onChange={this.longChange.bind(this)} value={range.long}/>
			</td>);
		} else {
			return (<td id={id + "Value"}>{`${range.short} / ${range.medium} / ${range.long}`}</td>)
		}
	}

	shortChange(e) {
		let short   = e.target.value;
		let range   = this.state.range;
		range.short = short;
		this.setState({
			range
		});
		this.props.onChange(range);
	}
}

export default RangeCell;