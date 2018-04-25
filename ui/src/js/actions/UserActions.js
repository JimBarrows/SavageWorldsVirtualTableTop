import axios from 'axios';
import constants from '../constants';
import {checkHttpStatus, parseJSON, convertErrorToString} from '../utils';
import {push} from 'react-router-redux';

let {
		    API_RESULT_SUCCESS,
		    API_RESULT_FAILURE,
		    API_STATUS_FINISHED,
		    API_STATUS_STARTED,
		    REGISTER_USER_BEGINS,
		    REGISTER_USER_FAILURE,
		    LOGIN_USER_BEGINS,
		    LOGIN_USER_SUCCESS,
		    USER_LOGGED_OUT,
		    USER_LOGOUT_FAILURE
    } = constants;


export function userRegister(username, password) {
	return function (dispatch) {

		dispatch({
			type: REGISTER_USER_BEGINS
			, payload: {
				status: API_STATUS_STARTED
			}
		});

		axios.post('/api/user', {
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
								error: convertErrorToString(data.error)
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
						localStorage.setItem('token', data.token);
						dispatch(push('/'));
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

export function userLogin(username, password, redirect = '/') {
	return function (dispatch) {
		dispatch({
			type: LOGIN_USER_BEGINS,
			payload: {
				status: API_STATUS_STARTED
			}
		});

		return axios.post('/api/user/authenticate', {
					username
					, password
				})
				.then(checkHttpStatus)
				.then(parseJSON)
				.then(function (data) {
					dispatch({
						type: LOGIN_USER_SUCCESS,
						payload: {
							token: data.token,
							status: API_STATUS_FINISHED,
							result: API_RESULT_SUCCESS,
						}
					});
					axios.defaults.headers = {
						'x-access-token': data.token
					};
					localStorage.setItem('token', data.token);
					dispatch(push('/'));
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
				});
	}
}

export function logout() {
	return function (dispatch) {
		axios.get('/api/user/logout')
				.then(checkHttpStatus)
				.then(parseJSON)
				.then(function (data) {
					axios.defaults.headers = {
						'x-access-token': '"
					};
					localStorage.removeItem('token');
					dispatch({
						type: USER_LOGGED_OUT
					});
					store.dispatch(push("/"));
				})
				.catch(function (error) {
					dispatch({
						type: USER_LOGOUT_FAILURE
						, payload: {
							status: API_STATUS_FINISHED,
							result: API_RESULT_FAILURE,
							error: convertErrorToString(error)
						}
					});
				});
	}
}

export function loginUserSuccess(token) {
	localStorage.setItem('token', token);
	axios.defaults.headers = {
		"x-access-token": token
	};
	return {
		type: LOGIN_USER_SUCCESS,
		payload: {
			token: token
		}
	}
}