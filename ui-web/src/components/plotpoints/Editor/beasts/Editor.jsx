import {NumberFormGroup,} from 'bootstrap-react-components'
import PropTypes          from 'prop-types'
import React              from 'react'
import CharacterSheet     from '../../../PlotPointEditor/components/character_sheet/index'
import SpecialAbilities   from './special_abilities/index'


export default class Editor extends React.Component {

	static propTypes = {
		id             : PropTypes.string.isRequired,
		index          : PropTypes.number.isRequired,
		item           : PropTypes.object.isRequired,
		skillsAvailable: PropTypes.array.isRequired
	}

	state = {
		selected: ''
	}

	armorChange            = e => this.props.onChange(Object.assign({}, this.props.item, {armor: parseInt(e.target.value, 10)}), this.props.index)
	onChange               = item => this.props.onChange(item, this.props.index)
	specialAbilitiesChange = specialAbilities => {this.props.onChange(Object.assign({}, this.props.item, {specialAbilities}), this.props.index)}

	render() {
		let component_id = `BeastEditor-${this.props.index}-${this.props.id}`
		return (
			<CharacterSheet id={component_id} index={this.props.index} item={this.props.item}
			                skillsAvailable={this.props.skillsAvailable} onChange={this.onChange}>
				<NumberFormGroup id={component_id + '-Armor'} label={'Armor'} value={this.props.item.armor || 0}
				                 onChange={this.armorChange}/>
				<SpecialAbilities abilities={this.props.item.specialAbilities} id={component_id}
				                  onChange={this.specialAbilitiesChange}/>
			</CharacterSheet>
		)
	}
}
