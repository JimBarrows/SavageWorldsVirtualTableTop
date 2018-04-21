import {plotPoint_constants} from "../constants";
import {createReducer} from "../utils/index";

let {PLOT_POINT_EDIT, PLOT_POINT_UPDATE_SUCCESS, PLOT_POINT_NEW} = plotPoint_constants;

const initialState = {
	plotPoint: {
		name                  : "",
		description           : "",
		maximumMinorHindrances: 2,
		maximumMajorHindrances: 1,
		maximumAttributePoints: 5,
		maximumSkillPoints    : 15,
		ammunition            : [],
		arcaneBackgrounds     : [],
		races                 : []
	}

};

export default createReducer(initialState, {
	[PLOT_POINT_EDIT]          : (state, payload) => Object.assign({}, {
		plotPoint: payload.plotPoint
	}),
	[PLOT_POINT_UPDATE_SUCCESS]: (state, payload) => Object.assign({}, {
		plotPoint: payload.plotPoint
	}),
	[PLOT_POINT_NEW]           : (state, payload) => Object.assign({}, {
		plotPoint: payload.plotPoint
	})
})
;