import {Panel, PanelBody, PanelHeader, RemoveButton} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';
import NumberFormGroup from './NumberFormGroup';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class MundaneItemEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps      = {
		id: 'MundaneItemEditorComponent'
	};
	       costChange        = e => this.props.onChange(Object.assign({}, this.props.mundaneItem, {cost: parseInt(e.target.value)}), this.props.index);
	       descriptionChange = e => this.props.onChange(Object.assign({}, this.props.mundaneItem, {description: e.target.value}), this.props.index);
	       nameChange        = e => this.props.onChange(Object.assign({}, this.props.mundaneItem, {name: e.target.value}), this.props.index);
	       onDelete          = event => {
		       event.preventDefault();
		       this.props.onDelete(this.props.index);
	       };
	       weightChange      = e => this.props.onChange(Object.assign({}, this.props.mundaneItem, {name: e.target.value}), this.props.index);

	render() {
		return (
				<Panel id={this.props.id}>
					<PanelHeader id={this.props.id}>
						<div className={'btn-group pull-right'}>
							<RemoveButton id={this.props.id} onClick={this.onDelete}/>
						</div>
					</PanelHeader>
					<PanelBody id={this.props.id}>
						<TextFormGroup id='mundaneItemName' label='Name' onChange={this.nameChange} required={true}
						               value={this.props.mundaneItem.name}/>
						<TextAreaFormGroup id={"mundaneItemDescription"}
						                   label="Description"
						                   onChange={this.descriptionChange}
						                   value={this.props.mundaneItem.description}/>
						<NumberFormGroup id={'mundaneItemCost'} label='Cost' onChange={this.costChange} required={true}
						                 value={this.props.mundaneItem.cost}/>
						<NumberFormGroup id={'mundaneItemWeight'} label='Weight' onChange={this.weightChange} required={true}
						                 value={this.props.mundaneItem.weight}/>
					</PanelBody>
				</Panel>
		);
	}
}

