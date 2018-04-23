import {plotPoint_constants} from "../constants";
import {createReducer} from "../utils/index";

let {
	    PLOT_POINT_CANCEL,
	    PLOT_POINT_DESCRIPTION_CHANGE,
	    PLOT_POINT_MAXIMUM_ATTRIBUTE_POINT_CHANGE,
	    PLOT_POINT_MAXIMUM_MAJOR_HINDRANCE_CHANGE,
	    PLOT_POINT_MAXIMUM_MINOR_HINDRANCE_CHANGE,
	    PLOT_POINT_MAXIMUM_SKILL_POINTS_CHANGE
    } = plotPoint_constants;

const initialState = {
	name                  : "",
	description           : "",
	maximumMinorHindrances: 2,
	maximumMajorHindrances: 1,
	maximumAttributePoints: 5,
	maximumSkillPoints    : 15,
	ammunition            : [],
	arcaneBackgrounds     : [],
	races                 : []
};

export default createReducer(initialState, {
	[PLOT_POINT_CANCEL]: (state, payload) => Object.assign({}, {
		name                  : "",
		description           : "",
		maximumMinorHindrances: 2,
		maximumMajorHindrances: 1,
		maximumAttributePoints: 5,
		maximumSkillPoints    : 15,
		ammunition            : [],
		arcaneBackgrounds     : [],
		races                 : []
	}),

	[PLOT_POINT_DESCRIPTION_CHANGE]: (state, payload) => Object.assign({}, {
		description: payload.description
	}),

	[PLOT_POINT_MAXIMUM_ATTRIBUTE_POINT_CHANGE]: (state, payload) => Object.assign({}, {
		maximumAttributePoints: payload.maximumAttributePoints
	}),

	[PLOT_POINT_MAXIMUM_MAJOR_HINDRANCE_CHANGE]: (state, payload) => Object.assign({}, {
		maximumMajorHindrances: payload.maximumMajorHindrances
	}),

	[PLOT_POINT_MAXIMUM_MINOR_HINDRANCE_CHANGE]: (state, payload) => Object.assign({}, {
		maximumMinorHindrances: payload.maximumMinorHindrances
	}),

	[PLOT_POINT_MAXIMUM_SKILL_POINTS_CHANGE]: (state, payload) => Object.assign({}, {
		maximumSkillPoints: payload.maximumSkillPoints
	})
})
;