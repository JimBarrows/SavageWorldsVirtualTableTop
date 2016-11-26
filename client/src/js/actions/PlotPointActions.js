/**
 * Created by JimBarrows on 7/9/16.
 */
'use strict';

import {PlotPointEvent} from "../constants";
// import PlotPointStore from "../stores/PlotPointStore";

export function addRace(race) {
	// let plotPoint = PlotPointStore.current;
	// plotPoint.races.push(race);
	// dispatcher.dispatch({
	// 	type: PlotPointEvent.UPDATE_SUCCESS,
	// 	plotPoint
	// })
}

export function create(plotPoint) {
	// delete plotPoint._id;
	// axios.post('/api/plotPoints', plotPoint)
	// 		.then((response) => dispatcher.dispatch({
	// 			type: PlotPointEvent.SAVE_SUCCESS,
	// 			plotPoint: response.data
	// 		}))
	// 		.catch((error) => dispatcher.dispatch({
	// 			type: PlotPointEvent.SAVE_SUCCESS,
	// 			error
	// 		}));
}

export function editPlotPoint(id) {
	let plotPoint = PlotPointStore.findById(id);
	if (plotPoint) {
		// dispatcher.dispatch({
		// 	type: PlotPointEvent.EDIT,
		// 	plotPoint
		// })
	} else {
		// dispatcher.dispatch({
		// 	type: PlotPointEvent.NOT_FOUND,
		// 	id
		// })
	}

}

export function load() {
	// axios.get('/api/plotPoints')
	// 		.then((response) =>
	// 				dispatcher.dispatch({
	// 					type: PlotPointEvent.LOAD_SUCCESS,
	// 					plotPoints: response.data
	// 				})
	// 		)
	// 		.catch((error) => dispatcher.dispatch({
	// 			type: PlotPointEvent.LOAD_FAILURE,
	// 			error: error
	// 		}));
}

export function newPlotPoint() {
	let plotPoint = {
		name: "",
		description: "",
		races: [{
			name: "Human",
			description: "Human!",
			abilities: [{
				name: "Extra Edge",
				description: "Get one extra edge",
				cost: 2
			}]
		}]
	};
	// dispatcher.dispatch({
	// 	type: PlotPointEvent.NEW,
	// 	plotPoint
	// });
}

export function remove(plotPoint) {
	// axios.delete('/api/plotPoints/' + plotPoint._id)
	// 		.then((response) => dispatcher.dispatch({
	// 			type: PlotPointEvent.REMOVE_SUCCESS,
	// 			plotPoint: plotPoint
	// 		}))
	// 		.catch((error) => dispatcher.dispatch({
	// 			type: PlotPointEvent.REMOVE_FAILURE,
	// 			error: error.status
	// 		}));
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
	// axios.put('/api/plotPoints/' + plotPoint._id, plotPoint)
	// 		.then((response) => dispatcher.dispatch({
	// 			type: PlotPointEvent.SAVE_SUCCESS,
	// 			plotPoint: response.data
	// 		}))
	// 		.catch((error) => dispatcher.dispatch({
	// 			type: PlotPointEvent.SAVE_SUCCESS,
	// 			error: error.status
	// 		}));
}
