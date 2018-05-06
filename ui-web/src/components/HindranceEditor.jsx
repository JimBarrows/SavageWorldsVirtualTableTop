import {Panel, PanelBody, PanelHeader, RemoveButton, SelectFormGroup} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class HindranceEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps = {
		id: 'HindranceEditor'
	};

	descriptionChange = e => this.props.onChange({
		severity   : this.props.hindrance.severity,
		description: e.target.value,
		name       : this.props.hindrance.name,
	}, this.props.index);

	nameChange = e => this.props.onChange({
		severity   : this.props.hindrance.severity,
		description: this.props.hindrance.description,
		name       : e.target.value,
	}, this.props.index);

	onDelete       = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	severityChange = e => this.props.onChange({
		description: this.props.hindrance.description,
		name       : this.props.hindrance.name,
		severity   : e.target.value
	}, this.props.index);

	render() {
		return (
				<Panel id={this.props.id}>
					<PanelHeader id={this.props.id}>
						<div className={'btn-group pull-right'}>
							<RemoveButton id={this.props.id} onClick={this.onDelete}/>
						</div>
					</PanelHeader>
					<PanelBody id={this.props.id}>
						<TextFormGroup id='hindranceName' label='Name' onChange={this.nameChange} required={true}
						               value={this.props.hindrance.name}/>
						<SelectFormGroup id={"hindranceSeverity"} label={'Severity'} onChange={this.severityChange}
						                 options={[{label: 'Minor', value: 'Minor'},
							                 {label: 'Major', value: 'Major'},
							                 {label: 'Major or Minor', value: 'Major or Minor'}]}
						                 value={this.props.hindrance.severity}/>
						<TextAreaFormGroup id={'hindranceDescription'} label={'Description'} onChange={this.descriptionChange}
						                   value={this.props.hindrance.description}/>
					</PanelBody>
				</Panel>
		);
	}
}

