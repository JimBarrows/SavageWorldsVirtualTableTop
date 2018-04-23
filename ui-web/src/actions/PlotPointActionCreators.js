/**
 * Created by JimBarrows on 4/23/18.
 */

import {plotPoint_constants} from '../constants';

export function cancelChanges() {
	return {
		type: plotPoint_constants.PLOT_POINT_CANCEL
	};
}

export function descriptionChange(description) {
	return {
		type   : plotPoint_constants.PLOT_POINT_DESCRIPTION_CHANGE,
		payload: {
			description
		}
	};
}