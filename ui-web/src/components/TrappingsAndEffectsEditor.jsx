import React from 'react';
import BaseEditor from './BaseEditor';
import EditorList from './EditorList';
import EffectsEditor from './EffectsEditor';
import TextAreaFormGroup from './TextAreaFormGroup';
import TextFormGroup from './TextFormGroup';

export default class TrappingsAndEffectsEditor extends React.Component {

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	effectsChange     = effects => this.props.onChange(Object.assign({}, this.props.item, {effects}), this.props.index);
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	onDelete          = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='arcaneBackgroundName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<TextAreaFormGroup id={"arcaneBackgroundDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
					<EditorList
							emptyItem={({
								name       : '',
								description: ''
							})}
							headingLevel={3}
							id={'effectsEditorList'}
							list={this.props.item.effects}
							onChange={this.effectsChange}
							title={'Effects'}>
						<EffectsEditor/>
					</EditorList>
				</BaseEditor>
		);
	}
}
