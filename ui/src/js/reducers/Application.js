/**
 * Created by JimBarrows on 11/26/16.
 */
import constants from "../constants";

const initialState = {
	isLoading: false,
	message: {
		show: false,
		context: "",
		message: ""
	}
};

let {
		    API_RESULT_FAILURE,
		    API_RESULT_SUCCESS,
		    API_STATUS_FINISHED,
		    API_STATUS_STARTED,
		    DISPLAY_MESSAGE,
		    MESSAGE_CONTEXT_DANGER
    } = constants;

export default function app(state = initialState, action) {
	switch (action.type) {
		case DISPLAY_MESSAGE :
			return Object.assign({}, state, {
				message: {
					show: true,
					context: action.payload.context,
					message: action.payload.message
				}
			});

		default:
			if (action.payload && action.payload.status && action.payload.result) {
				switch (action.payload.status) {
					case API_STATUS_STARTED :
						return Object.assign({}, state, {
							isLoading: true
						});
					case API_STATUS_FINISHED:
						switch (action.payload.result) {
							case API_RESULT_SUCCESS :
								return Object.assign({}, state, {
									isLoading: false,
									message: initialState.message
								});
							case API_RESULT_FAILURE:
								return Object.assign({}, state, {
									isLoading: false,
									message: {
										show: true,
										context: MESSAGE_CONTEXT_DANGER,
										message: action.payload.error
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
			}
			else {
				return state;
			}
	}
	return state;
}