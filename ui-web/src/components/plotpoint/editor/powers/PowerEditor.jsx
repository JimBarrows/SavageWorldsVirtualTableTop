import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import BaseEditor from '../../../BaseEditor'
import RankSelectFormGroup from '../../../formgroups/RankSelectFormGroup'
import EditorList from '../../../EditorList'
import ModifierEditor from './ModifierEditor'

export default class PowerEditor extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired,
		index: PropTypes.number.isRequired,
		item: PropTypes.shape({
			description: PropTypes.string,
			duration: PropTypes.string,
			name: PropTypes.string,
			powerPoints: PropTypes.number,
			range: PropTypes.string,
			rank: PropTypes.string,
			trappings: PropTypes.string,
			modifiers: PropTypes.array
		}).isRequired,
		onChange: PropTypes.func.isRequired,
		onDelete: PropTypes.func.isRequired
	}

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	durationChange = e => this.props.onChange(Object.assign({}, this.props.item, {duration: e.target.value}), this.props.index);
	nameChange = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	onDelete = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	powerPointChange = e => {
		const value = parseInt(e.target.value, 10);
		if (value > 0) {
			this.props.onChange(Object.assign({}, this.props.item, {powerPoints: value}), this.props.index);
		}
	};
	rangeChange = e => this.props.onChange(Object.assign({}, this.props.item, {range: e.target.value}), this.props.index);
	rankChange = e => this.props.onChange(Object.assign({}, this.props.item, {rank: e.target.value}), this.props.index);
	trappingsChange = e => this.props.onChange(Object.assign({}, this.props.item, {trappings: e.target.value}), this.props.index);
	
	modifiersChange = (modifiers) => {
		this.props.onChange(Object.assign({}, this.props.item, {modifiers: modifiers}), this.props.index);
	};

	render() {
		const {item} = this.props;
		const modifiers = item.modifiers || [];
		
		return (
			<BaseEditor id={this.props.id} onDelete={this.onDelete}>
				<TextFormGroup 
					id={`FormControl-text-TextFormGroup-PowerEditor-Powers-${this.props.id}-${this.props.index}-Name`}
					label='Name' 
					onChange={this.nameChange} 
					required={true}
					value={item.name}
				/>
				<TextAreaFormGroup 
					id={`TextAreaFormGroup-PowerEditor-Powers-${this.props.id}-${this.props.index}-Description`}
					label="Description"
					onChange={this.descriptionChange}
					value={item.description}
				/>
				<RankSelectFormGroup 
					id={`powerRank-${this.props.index}`} 
					onChange={this.rankChange} 
					rank={item.rank} 
					required={true}
				/>
				<NumberFormGroup 
					id={`FormControl-number-NumberFormGroup-PowerEditor-Powers-${this.props.id}-${this.props.index}-PowerPoints`}
					label={'Power Points'} 
					onChange={this.powerPointChange}
					required={true} 
					value={item.powerPoints}
					min={1}
				/>
				<TextFormGroup 
					id={`FormControl-text-TextFormGroup-PowerEditor-Powers-${this.props.id}-${this.props.index}-Range`}
					label='Range' 
					onChange={this.rangeChange} 
					required={true}
					value={item.range}
				/>
				<TextFormGroup 
					id={`FormControl-text-TextFormGroup-PowerEditor-Powers-${this.props.id}-${this.props.index}-Duration`}
					label='Duration' 
					onChange={this.durationChange} 
					required={true}
					value={item.duration}
				/>
				<TextFormGroup 
					id={`FormControl-text-TextFormGroup-PowerEditor-Powers-${this.props.id}-${this.props.index}-Trappings`}
					label='Trappings' 
					onChange={this.trappingsChange}
					value={item.trappings}
				/>
				
				<h4>Modifiers</h4>
				<EditorList
					emptyItem={{
						name: '',
						description: '',
						powerPointModifier: 0
					}}
					id={`Modifiers-PowerEditor-Powers-${this.props.id}-${this.props.index}`}
					list={modifiers}
					onChange={this.modifiersChange}
					title={'Modifiers'}>
					<ModifierEditor />
				</EditorList>
			</BaseEditor>
		);
	}
}