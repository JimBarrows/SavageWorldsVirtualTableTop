import {SelectFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes                                           from 'prop-types'
import React                                               from 'react'
import BaseEditor                                          from '../../../BaseEditor'

export default class Editor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired,
		onChange: PropTypes.func.isRequired,
		onDelete: PropTypes.func.isRequired,
		item: PropTypes.shape({
			name: PropTypes.string,
			severity: PropTypes.string,
			description: PropTypes.string
		}).isRequired,
		index: PropTypes.number.isRequired
	};

	static defaultProps = {
		id: 'Editor'
	};

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);

	nameChange = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);

	onDelete       = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	severityChange = e => this.props.onChange(Object.assign({}, this.props.item, {severity: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='hindranceName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<SelectFormGroup id={"hindranceSeverity"} label={'Severity'} onChange={this.severityChange}
					                 options={[{label: 'Minor', value: 'Minor'},
						                 {label: 'Major', value: 'Major'},
						                 {label: 'Major or Minor', value: 'Major or Minor'}]}
					                 value={this.props.item.severity}/>
					<TextAreaFormGroup id={'hindranceDescription'} label={'Description'} onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
				</BaseEditor>
		);
	}
}

