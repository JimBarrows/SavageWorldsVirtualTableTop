import {FormGroup, TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import React from "react";
import {connect} from "react-redux";
import {ItemEditor} from "../Item";

class EdgeEditor extends ItemEditor {

	edgeTypeChange(e) {
		let edgeType = this.props.edgeTypes.find((et) => et._id === e.target.value);
		this.setState({
			edgeType
		})

	}

	propsToState(props) {
		let {_id, name, description, edgeType} = props;
		this.setState({
			_id, name, description, edgeType
		});
	}

	render() {
		let {_id, name, description, edgeType} = this.state;
		return (
				<div id="EdgeEditorForm">
					<TextFormGroup
							label="Edge Name"
							onChange={this.nameChange.bind(this)}
							value={name}
							required={true}/>
					<FormGroup label="Edge Type" id="edgeType" required={true}>
						<select class="form-control" id="edgeTypeSelect" onChange={this.edgeTypeChange.bind(this)}
						        value={edgeType && edgeType._id ? edgeType._id : ""}>
							{this.props.edgeTypes.map((et) => <option key={et._id} value={et._id}>{et.name}</option>)}
						</select>
					</FormGroup>
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
		let {_id, name, description, edgeType = this.props.edgeTypes[0]} = this.state;
		return {_id, name, description, edgeType};
	}

}

const mapStateToProps = (state) => {
	return {
		edgeTypes: state.PlotPoint.plotPoint.edgeTypes,
		edges: state.PlotPoint.plotPoint.edges
	};
};

const mapDispatchToProps = (dispatch) => {
	return {};
};

export default connect(mapStateToProps, mapDispatchToProps)(EdgeEditor);