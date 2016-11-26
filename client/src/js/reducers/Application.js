/**
 * Created by JimBarrows on 11/26/16.
 */
import {DISPLAY_MESSAGE, API_STATUS, API_RESULT} from "../constants";

const initialState = {
	isLoading: false,
	message: {
		show: false,
		context: "",
		message: ""
	}
};


export default function app(state = initialState, action) {
	switch (action.type) {
		case DISPLAY_MESSAGE :
			return Object.assign({}, state, {
				message: {
					show: true,
					context: action.context,
					message: action.message
				}
			});
	}
	if (action.status && action.result) {
		switch (action.status) {
			case API_STATUS.started :
				return Object.assign({}, state, {
					isLoading: true
				});
			case API_STATUS.finished:
				switch (action.result) {
					case API_RESULT.success :
						return Object.assign({}, state, {
							isLoading: false,
							message: initialState.app.message
						});
					case API_RESULT.error:
						return Object.assign({}, state, {
							isLoading: false,
							message: {
								show: true,
								context: MESSAGE_CONTEXT.danger,
								message: action.error
							}
						});
					case API_RESULT.timeout:
						return Object.assign({}, state, {
							isLoading: false,
							message: {
								show: true,
								context: MESSAGE_CONTEXT.danger,
								message: action.error || "Could not retrieve data in time, please try again"
							}

						});
						break;
					default:
						return state;
				}
				break;
			default:
				return state;
		}
	} else {
		return state;
	}
}