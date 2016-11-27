import axios from "axios";
import constants from "../constants";
import {checkHttpStatus, parseJSON, convertErrorToString} from "../utils";
import {push} from "react-router-redux";

console.log("constants: ", constants);
let {
		    API_RESULT_SUCCESS,
		    API_RESULT_FAILURE,
		    API_STATUS_FINISHED,
		    API_STATUS_STARTED,
		    REGISTER_USER_BEGINS,
		    REGISTER_USER_FAILURE,
		    LOGIN_USER_SUCCESS
    } = constants;


export function userRegister(username, password) {
	return function (dispatch) {

		dispatch({
			type: REGISTER_USER_BEGINS
			, payload: {
				status: API_STATUS_STARTED
			}
		});

		axios.post("/api/user", {
					username
					, password
				})
				.then(checkHttpStatus)
				.then(parseJSON)
				.then((data) => {
					if (data.error) {
						dispatch({
							type: REGISTER_USER_FAILURE
							, payload: {
								status: API_STATUS_FINISHED,
								result: API_RESULT_FAILURE,
								error: convertErrorToString(response.data.error)
							}
						});
					} else {
						dispatch({
							type: LOGIN_USER_SUCCESS,
							payload: {
								token: data.token,
								status: API_STATUS_FINISHED,
								result: API_RESULT_SUCCESS,
							}
						});
						dispatch(push("/"));
					}
				})
				.catch(function (error) {
					dispatch({
						type: REGISTER_USER_FAILURE
						, payload: {
							status: API_STATUS_FINISHED,
							result: API_RESULT_FAILURE,
							error: convertErrorToString(error)
						}
					});
				})
	}
}

export function login(username, password, redirect = "/") {
	return function (dispatch) {
		dispatch(loginUserRequest());

		return axios.post("/api/user/login", {
					username
					, password
				})
				.then(function (response) {
					dispatch.dispatch({
						type: UserEventNames.USER_LOGGED_IN
						, username: response.data.username
						, id: response.data.id
					})
				})
				.catch(function (error) {
					dispatcher.dispatch({
						type: UserEventNames.USER_LOGIN_FAILURE
						, username
						, error: error.data
					})
				});
	}
}

export function logout() {
	axios.get("/api/user/logout")
			.then(function (response) {
				dispatcher.dispatch({
					type: UserEventNames.USER_LOGGED_OUT
				});
			})
			.catch(function (error) {
				dispatcher.dispatch({
					type: UserEventNames.USER_LOGOUT_FAILURE
					, error: error.data
				})
			})
}

export function loginUserSuccess(token) {
	localStorage.setItem('token', token);
	return {
		type: LOGIN_USER_SUCCESS,
		payload: {
			token: token
		}
	}
}