/**
 * Created by JimBarrows on 7/9/16.
 */
import axios from "axios";
import {application_constants, plotPoint_constants} from '../constants';
import {checkHttpStatus, parseJSON, convertErrorToString} from "../utils";


let {
	    API_STATUS_FINISHED, API_RESULT_FAILURE, API_RESULT_SUCCESS, API_STATUS_STARTED
    } = application_constants;

let {
	    PLOT_POINT_DELETE_BEGIN, PLOT_POINT_DELETE_SUCCESS, PLOT_POINT_DELETE_FAILURE, PLOT_POINT_NEW, PLOT_POINT_UPDATE_FAILURE, PLOT_POINT_UPDATE_BEGIN, PLOT_POINT_UPDATE_SUCCESS, PLOT_POINT_EDIT,
	    PLOT_POINT_NOT_FOUND, PLOT_POINTS_LOAD_FAILURE, PLOT_POINTS_LOAD_SUCCESS, PLOT_POINTS_LOAD_BEGIN,
	    PLOT_POINT_ADD_BEGIN, PLOT_POINT_ADD_SUCCESS, PLOT_POINT_ADD_FAILURE
    } = plotPoint_constants;

export function loadPage(pageNumber) {
	let pageNo = pageNumber ||0;
	return function (dispatch) {

		dispatch({
			type   : PLOT_POINTS_LOAD_BEGIN,
			payload: {
				status: API_STATUS_STARTED
			}
		});

		axios.get(`/api/plotPoints?size=10&sort=name,asc&page=${pageNo}`)
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

export function loadNextPage() {
	return function (dispatch, getState) {

		dispatch({
			type   : PLOT_POINTS_LOAD_BEGIN,
			payload: {
				status: API_STATUS_STARTED
			}
		});

		axios.get(getState().PlotPoints.links.next.href)
				.then(checkHttpStatus)
				.then(parseJSON)
				.then(data => dispatch({
					type : PLOT_POINTS_LOAD_SUCCESS,
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

export function loadPreviousPage() {
	return function (dispatch, getState) {

		dispatch({
			type   : PLOT_POINTS_LOAD_BEGIN,
			payload: {
				status: API_STATUS_STARTED
			}
		});

		axios.get(getState().PlotPoints.links.prev.href)
				.then(checkHttpStatus)
				.then(parseJSON)
				.then(data => dispatch({
					type : PLOT_POINTS_LOAD_SUCCESS,
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
