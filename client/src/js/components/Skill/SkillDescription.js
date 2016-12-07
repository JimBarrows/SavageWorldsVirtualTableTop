import {FormGroup, TextFormGroup, TextAreaFormGroup} from "bootstrap-react-components";
import React from "react";

const emptySkill = {
	name: "",
	description: "",
	attribute: "",
	_id: ""
};
class SkillDescription extends React.Component {

	attributeChange(e) {
		this.setState({
			attribute: e.target.value
		});
	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(nextProps) {
		this.propsToState(nextProps);
	}

	editing(e) {
		this.setState({
			editing: true
		})
	}

	descriptionChange(e) {
		this.setState({
			description: e.target.value
		});
	}

	nameChange(e) {
		this.setState({
			name: e.target.value
		});
	}

	propsToState(props) {
		let {item                               = emptySkill, index = 0, adding = false}   = props;
		let {_id, name, description, attribute} = item;
		this.setState({
			_id, name, description, attribute, index, adding
		});
	}

	remove() {
		let {_id, name, description, attribute} = this.state;
		this.props.remove({_id, name, description, attribute});
	}

	render() {
		let {_id, name, description, attribute, index, adding, editing} = this.state;
		let idName                                                      = name || "NewSkill";
		let idKey                                                       = _id || `${idName.replace(" ", "_")}_${index}`;
		let buttonName                                                  = adding ? "Add" : "Save";
		let display                                                     = adding || editing ?
				<div id={idKey}>
					<TextFormGroup label="Skill Name" name="name" onChange={this.nameChange.bind(this)} value={name}/>
					<TextAreaFormGroup label="Skill Description" name="description" onChange={this.descriptionChange.bind(this)}
					                   value={description}/>
					<FormGroup label="Attribute" id="attribute" error={""} required={true}>
						<select class="form-control" id="attributeSelect" onChange={this.attributeChange.bind(this)}
						        value={attribute}>
							<option>Agility</option>
							<option>Smarts</option>
							<option>Strength</option>
							<option>Spirit</option>
							<option>Vigor</option>
						</select>
					</FormGroup>
					<button type="button" class="btn btn-default" onClick={this.save.bind(this)}>{buttonName}</button>
				</div>
				:
				<div id={idKey}>
					<dt>{name}
						<small>({attribute})</small>
						<div class="btn-group" role="group" aria-label="...">
							<button type="button" class="btn btn-default btn-xs" onClick={this.editing.bind(this)}><span
									class="glyphicon glyphicon-pencil" aria-hidden="true"/></button>
							<button type="button" class="btn btn-danger btn-xs" onClick={this.remove.bind(this)}><span
									class="glyphicon glyphicon-remove" aria-hidden="true"/></button>
						</div>
					</dt>
					<dd>{description}</dd>
				</div>;
		return display;
	}

	save() {
		let {_id, name, description, attribute} = this.state;
		if (this.state.adding) {
			this.props.save({
				_id, name, description, attribute
			});
		} else {
			this.props.update({
				_id, name, description, attribute
			});
			this.setState({
				editing: false
			})
		}

	}
}

export default SkillDescription;