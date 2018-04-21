/**
 * Created by JimBarrows on 7/9/16.
 */
import axios from "axios";
import {application_constants, plotPoint_constants} from '../constants';
import {push} from "react-router-redux";
import {checkHttpStatus, parseJSON, convertErrorToString} from "../utils";


let {
	    API_STATUS_FINISHED, API_RESULT_FAILURE, API_RESULT_SUCCESS, API_STATUS_STARTED
    } = application_constants;

let {
	    PLOT_POINT_DELETE_BEGIN, PLOT_POINT_DELETE_SUCCESS, PLOT_POINT_DELETE_FAILURE, PLOT_POINT_NEW, PLOT_POINT_UPDATE_FAILURE, PLOT_POINT_UPDATE_BEGIN, PLOT_POINT_UPDATE_SUCCESS, PLOT_POINT_EDIT,
	    PLOT_POINT_NOT_FOUND, PLOT_POINTS_LOAD_FAILURE, PLOT_POINTS_LOAD_SUCCESS, PLOT_POINTS_LOAD_BEGIN,
	    PLOT_POINT_ADD_BEGIN, PLOT_POINT_ADD_SUCCESS, PLOT_POINT_ADD_FAILURE
    } = plotPoint_constants;

export function addRace(race) {
	// let plotPoint = PlotPointStore.current;
	// plotPoint.races.push(race);
	// dispatcher.dispatch({
	// 	type: PlotPointEvent.UPDATE_SUCCESS,
	// 	plotPoint
	// })
}

export function create(plotPoint) {
	return function (dispatch) {
		delete plotPoint._id;
		dispatch({
			type   : PLOT_POINT_ADD_BEGIN,
			payload: {
				status: API_STATUS_STARTED
			}
		});

		axios.post('/api/plotPoints', plotPoint)
				.then(checkHttpStatus)
				.then(parseJSON)
				.then((data) => dispatch({
					type   : PLOT_POINT_ADD_SUCCESS,
					payload: {
						status                               : API_STATUS_FINISHED,
						result: API_RESULT_SUCCESS, plotPoint: data
					}
				}))
				.then(() => dispatch(push("/")))
				.catch((error) => dispatch({
					type   : PLOT_POINT_ADD_FAILURE,
					payload: {
						status: API_STATUS_FINISHED,
						result: API_RESULT_FAILURE,
						error : convertErrorToString(error)
					}
				}));
	};

}

export function loadPlotPoint(id) {
	return (dispatch, state) => {
		let plotPoint = state().PlotPoints.plotPoints.find((pp) => id === pp._id);
		if (plotPoint) {
			dispatch({
				type   : PLOT_POINT_EDIT,
				payload: {
					plotPoint
				}
			});
		} else {
			dispatch({
				type   : PLOT_POINT_NOT_FOUND,
				payload: {id}
			});
		}
	};
}

export function load() {
	return function (dispatch) {

		dispatch({
			type   : PLOT_POINTS_LOAD_BEGIN,
			payload: {
				status: API_STATUS_STARTED
			}
		});

		axios.get('/api/plotPoints?size=10&sort=name,asc')
				.then(checkHttpStatus)
				.then(parseJSON)
				.then((data) =>
						dispatch({
							type   : PLOT_POINTS_LOAD_SUCCESS,
							payload: {
								plotPoints: data._embedded.plotPoints,
								page      : data.page,
								links     : data._links,
								status    : API_STATUS_FINISHED,
								result    : API_RESULT_SUCCESS
							}
						}))
				.catch((error) =>
						dispatch({
							type   : PLOT_POINTS_LOAD_FAILURE,
							payload: {
								status: API_STATUS_FINISHED,
								result: API_RESULT_FAILURE,
								error : convertErrorToString(error)
							}
						}));
	};
}

export function newPlotPoint() {
	return function (dispatch) {
		let plotPoint = {
			name        : "New Plot Point",
			description : "",
			settingRules: [],
			races       : [{
				name       : "Human",
				description: "Human!",
				abilities  : [{
					name       : "Extra Edge",
					description: "Get one extra edge",
					cost       : 2
				}]
			}]
		};
		dispatch({
			type   : PLOT_POINT_NEW,
			payload: {plotPoint}
		});
	};

}

export function remove(plotPoint) {
	return function (dispatch, state) {
		dispatch({
			type   : PLOT_POINT_DELETE_BEGIN,
			payload: {
				status: API_STATUS_STARTED
			}
		});
		axios.delete('/api/plotPoint/' + plotPoint._id)
				.then(checkHttpStatus)
				.then(parseJSON)
				.then((data) => dispatch({
					type   : PLOT_POINT_DELETE_SUCCESS,
					payload: {
						status   : API_STATUS_FINISHED,
						result   : API_RESULT_SUCCESS,
						plotPoint: plotPoint
					}
				}))
				.then(() => dispatch(push("/")))
				.catch((error) => dispatch({
					type   : PLOT_POINT_DELETE_FAILURE,
					payload: {
						status: API_STATUS_FINISHED,
						result: API_RESULT_FAILURE,
						error : convertErrorToString(error)
					}
				}));
	};

}

export function removeRace(race) {
	// dispatcher.dispatch({
	// 	type: PlotPointEvent.REMOVE_RACE,
	// 	race
	// });
}

export function updateRace(race) {
	// dispatcher.dispatch({
	// 	type: PlotPointEvent.UPDATE_RACE,
	// 	race
	// })
}

export function update(plotPoint) {
	return (dispatch) => {
		dispatch({
			type   : PLOT_POINT_UPDATE_BEGIN,
			payload: {
				status: API_STATUS_STARTED
			}
		});
		axios.put('/api/plotPoint/' + plotPoint._id, plotPoint)
				.then(checkHttpStatus)
				.then(parseJSON)
				.then((data) => dispatch({
					type   : PLOT_POINT_UPDATE_SUCCESS,
					payload: {
						status   : API_STATUS_FINISHED,
						result   : API_RESULT_SUCCESS,
						plotPoint: plotPoint
					}
				}))
				.then(() => dispatch(push("/")))
				.catch((error) => dispatch({
					type   : PLOT_POINT_UPDATE_FAILURE,
					payload: {
						status: API_STATUS_FINISHED,
						result: API_RESULT_FAILURE,
						error : convertErrorToString(error)
					}
				}));
	};

}
