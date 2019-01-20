import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React                                               from 'react'
import BaseEditor                                          from '../../../BaseEditor'
import RankSelectFormGroup                                 from '../../../formgroups/RankSelectFormGroup'


export default class Form extends React.Component {

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
	durationChange    = e => this.props.onChange(Object.assign({}, this.props.item, {duration: e.target.value}), this.props.index);
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
	onDelete          = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};
	powerPointChange  = e => this.props.onChange(Object.assign({}, this.props.item, {powerPoints: parseInt(e.target.value, 10)}), this.props.index);
	rangeChange       = e => this.props.onChange(Object.assign({}, this.props.item, {range: e.target.value}), this.props.index);
	rankChange        = e => this.props.onChange(Object.assign({}, this.props.item, {rank: e.target.value}), this.props.index);

	render() {
		return (
				<BaseEditor id={this.props.id} onDelete={this.onDelete}>
					<TextFormGroup id='powerName' label='Name' onChange={this.nameChange} required={true}
					               value={this.props.item.name}/>
					<TextAreaFormGroup id={"powerDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={this.props.item.description}/>
					<RankSelectFormGroup id={'powerRank'} onChange={this.rankChange} rank={this.props.item.rank} required={true}/>
					<NumberFormGroup id={'powerPowerPoints'} label={'Power Points'} onChange={this.powerPointChange}
					                 required={true} value={this.props.item.powerPoints}/>
					<TextFormGroup id='powerRange' label='Range' onChange={this.rangeChange} required={true}
					               value={this.props.item.range}/>
					<TextFormGroup id='powerDuration' label='Duration' onChange={this.durationChange} required={true}
					               value={this.props.item.duration}/>
				</BaseEditor>
		);
	}
}
