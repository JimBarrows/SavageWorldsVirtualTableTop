import constants from "../constants";
import {createReducer} from "../utils";

let {PLOT_POINT_EDIT, PLOT_POINT_UPDATE_SUCCESS, PLOT_POINT_NEW} = constants;

const initialState = {
	plotPoint: {
		name: "",
		description: "",
		settingRules: []
	}

};

export default createReducer(initialState, {
	[PLOT_POINT_EDIT]: (state, payload) => Object.assign({}, {
		plotPoint: payload.plotPoint
	}),
	[PLOT_POINT_UPDATE_SUCCESS]: (state, payload) => Object.assign({}, {
		plotPoint: payload.plotPoint
	}),
	[PLOT_POINT_NEW]: (state, payload) => Object.assign({}, {
		plotPoint: payload.plotPoint
	})
})
;