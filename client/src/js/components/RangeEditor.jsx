import React from "react";
import {NumberFormGroup} from "bootstrap-react-components";

class RangeEditor extends React.Component {

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
			range: props.value
		});
	}

	render() {
		let {id, required}  = this.props;
		let {range          = {short: 0, medium: 0, long: 0}}  = this.state;
		return (
				<div id={id + "RangeEditor"} class="RangeEditor">
					<NumberFormGroup label="Short" onChange={this.shortChange.bind(this)} value={range.short}
					                 required={required}/>
					<NumberFormGroup label="Medium" onChange={this.mediumChange.bind(this)} value={range.medium}
					                 required={required}/>
					<NumberFormGroup label="Large" onChange={this.longChange.bind(this)} value={range.long} required={required}/>
				</div>
		);
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

export default RangeEditor;