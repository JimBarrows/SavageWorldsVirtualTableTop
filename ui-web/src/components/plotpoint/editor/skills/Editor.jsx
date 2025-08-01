import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes                          from 'prop-types'
import React                              from 'react'
import BaseEditor                         from '../../../BaseEditor'
import AttributeSelectFormGroup           from '../../../formgroups/AttributeSelectFormGroup'

class Editor extends React.Component {

	static propTypes = {
		id: PropTypes.string,
		index: PropTypes.number.isRequired,
		item: PropTypes.shape({
			name: PropTypes.string,
			attribute: PropTypes.string,
			description: PropTypes.string
		}).isRequired,
		onChange: PropTypes.func.isRequired,
		onDelete: PropTypes.func.isRequired
	};

	static defaultProps = {
		id: 'Editor'
	};

	attributeChange = e => this.props.onChange(Object.assign({}, this.props.item, {attribute: e.target.value}), this.props.index);

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);

	nameChange = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);

	onDelete = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};

	render() {
		return (

				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='skillName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<AttributeSelectFormGroup id={'skillAttribute'}
					                          onChange={this.attributeChange}
					                          attribute={this.props.item.attribute}/>
					<TextAreaFormGroup id={'skillDescription'} label={'Description'} onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
				</BaseEditor>
		);
	}
}

export default Editor
