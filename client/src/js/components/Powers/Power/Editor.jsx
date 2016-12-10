import {NumberFormGroup, TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import React from "react";
import {ItemEditor} from "../../Item";
import RangeEditor from "../../RangeEditor";
import RankSelector from "../../RankSelector";

class PowerEditor extends ItemEditor {

	durationChange(e) {
		this.setState({
			duration: e.target.value
		});
	}

	maintenanceChange(e) {
		this.setState({
			maintenance: e.target.value
		});
	}

	propsToState(props) {
		let {_id, name, description, level, duration, maintenance, powerPoints, range, trappings} = props;
		this.setState({
			_id, name, description, level, duration, maintenance, powerPoints, range, trappings
		});
	}

	powerPointsChange(e) {
		this.setState({
			powerPoints: e.target.value
		});
	}

	rangeChange(range) {
		this.setState({
			range
		});
	}

	levelChange(e) {
		this.setState({
			level: e.target.value
		});
	}

	render() {
		let {
				    _id, name, description, level, duration, maintenance, powerPoints, range = {
			short: 0,
			medium: 0,
			long: 0
		}, trappings
		    }                                                                            = this.state;
		return (
				<div id="PowerEditorForm">
					<TextFormGroup
							label="Name"
							onChange={this.nameChange.bind(this)}
							value={name}
							required={true}/>
					<RankSelector
							label="Rank"
							onChange={this.levelChange.bind(this)}
							value={level}
							required={true}/>
					<NumberFormGroup
							label="Power Points"
							onChange={this.powerPointsChange.bind(this)}
							value={powerPoints}
							required={true}/>
					<RangeEditor
							onChange={this.rangeChange.bind(this)}
							value={range}
							required={true}/>
					<NumberFormGroup
							label="Duration"
							onChange={this.durationChange.bind(this)}
							value={duration}
							required={true}/>
					<NumberFormGroup
							label="Maintenance"
							onChange={this.maintenanceChange.bind(this)}
							value={maintenance}
							required={true}/>
					<TextFormGroup
							label="Trappings"
							onChange={this.trappingsChange.bind(this)}
							value={trappings}
							required={true}/>
					<TextAreaFormGroup
							label="Description"
							onChange={this.descriptionChange.bind(this)}
							value={description}
					/>
					<button type="button" class="btn btn-primary"
					        onClick={this.save.bind(this)}>Save
					</button>
					<button type="button" class="btn btn-default" onClick={this.cancel.bind(this)}>Cancel</button>
				</div>
		);
	}

	stateToItem() {
		let {_id, name, description, level, duration, maintenance, powerPoints, range, trappings} = this.state;
		return {_id, name, description, level, duration, maintenance, powerPoints, range, trappings};
	}

	trappingsChange(e) {
		this.setState({
			trappings: e.target.value
		});
	}
}

export default PowerEditor;