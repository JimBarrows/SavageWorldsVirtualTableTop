import {FormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components';
import React from 'react';
import {ItemEditor} from '../Item';

class HindranceEditor extends ItemEditor {

	propsToState(props) {
		let {_id, name, description, severity} = props;
		this.setState({
			_id, name, description, severity
		});
	}

	render() {
		let {_id, name, description, severity} = this.state;
		return (
				<div id='HindranceEditorForm'>
					<TextFormGroup
							label='Name'
							id={_id + 'Name'}
							onChange={this.nameChange.bind(this)}
							value={name}/>
					<TextAreaFormGroup
							label='Description'
							id={_id + 'Description'}
							onChange={this.descriptionChange.bind(this)}
							value={description}
					/>
					<FormGroup label='Severity'
					           id='severity'
					           required={true}>
						<select class='form-control'
						        id='severitySelect'
						        onChange={this.severityChange.bind(this)}
						        value={severity}>
							<option>Major</option>
							<option>Minor</option>
							<option>Major or Minor</option>
						</select>
					</FormGroup>
					<button type='button' class='btn btn-default'
					        onClick={this.save.bind(this)}>Save
					</button>
					<button type='button' class='btn btn-default' onClick={this.cancel.bind(this)}>Cancel</button>
				</div>
		);
	}

	severityChange(e) {
		this.setState({
			severity: e.target.value
		})
	}

	stateToItem() {
		let {_id, name, description, severity = 'Major'} = this.state;
		return {
			_id, name, description, severity
		}
	}
}

export default HindranceEditor;