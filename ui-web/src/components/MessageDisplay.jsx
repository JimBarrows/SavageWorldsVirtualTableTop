import React from 'react';
import PropTypes from 'prop-types';
import {application_constants} from '../constants';
import {connect} from "react-redux";

class MessageDisplay extends React.Component {

	render() {
		let contextClass = "alert-info";
		let {app}        = this.props;
		switch (app.message.context) {
			case application_constants.MESSAGE_CONTEXT_DANGER:
				contextClass = "alert-danger";
				break;
			case application_constants.MESSAGE_CONTEXT_INFO:
				contextClass = "alert-info";
				break;
			case application_constants.MESSAGE_CONTEXT_SUCCESS:
				contextClass = "alert-success";
				break;
			case application_constants.MESSAGE_CONTEXT_WARNING:
				contextClass = "alert-warning";
				break;
			default:
				contextClass = "alert-info";
		}
		let message = '';
		if (app.message.show) {
			message = <div id={'MessageDisplayComponent_' + this.props.id} className={`alert ${contextClass}`}
			               role="alert">{app.message.message}</div>;
		}
		return message;
	}
}

MessageDisplay.propTypes = {
	id: PropTypes.string.isRequired
};

MessageDisplay.defaultProps = {};

const mapStateToProps = (state, ownProps) => {
	return {
		app: state.app
	};
};

const mapDispatchToProps = (dispatch) => {
	return {};
};

export default connect(mapStateToProps, mapDispatchToProps)(MessageDisplay);