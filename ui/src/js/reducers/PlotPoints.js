/**
 * Created by JimBarrows on 11/28/16.
 */
import {createReducer} from "../utils";
import constants from "../constants";

let {PLOT_POINTS_LOAD_SUCCESS, PLOT_POINT_ADD_SUCCESS, PLOT_POINT_DELETE_SUCCESS} = constants;
const initialState                                                                = {
	plotPoints: []
};

export default createReducer(initialState, {
	[PLOT_POINT_ADD_SUCCESS]: (state, payload) => Object.assign({}, {
		plotPoints: [
			...state.plotPoints,
			payload.plotPoint
		]
	}),
	[PLOT_POINTS_LOAD_SUCCESS]: (state, payload) => Object.assign({}, {
		plotPoints: payload.plotPoints
	}),
	[PLOT_POINT_DELETE_SUCCESS]: (state, payload) => Object.assign({}, {
		plotPoints: [...state.plotPoints.filter((p) => p._id !== payload.plotPoint._id)]
	})
});
