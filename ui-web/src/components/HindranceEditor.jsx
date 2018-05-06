import {SelectFormGroup} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';
import BaseEditor from './BaseEditor';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class HindranceEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps = {
		id: 'HindranceEditor'
	};

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.hindrance, {description: e.target.value}), this.props.index);

	nameChange = e => this.props.onChange(Object.assign({}, this.props.hindrance, {name: e.target.value}), this.props.index);

	onDelete       = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	severityChange = e => this.props.onChange(Object.assign({}, this.props.hindrance, {severity: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='hindranceName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.hindrance.name}/>
					<SelectFormGroup id={"hindranceSeverity"} label={'Severity'} onChange={this.severityChange}
					                 options={[{label: 'Minor', value: 'Minor'},
						                 {label: 'Major', value: 'Major'},
						                 {label: 'Major or Minor', value: 'Major or Minor'}]}
					                 value={this.props.hindrance.severity}/>
					<TextAreaFormGroup id={'hindranceDescription'} label={'Description'} onChange={this.descriptionChange}
					                   value={this.props.hindrance.description}/>
				</BaseEditor>
		);
	}
}

