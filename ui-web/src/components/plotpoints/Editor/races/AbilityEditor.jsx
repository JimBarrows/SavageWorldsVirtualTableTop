import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components"
import PropTypes                                           from 'prop-types'
import React                                               from "react"
import BaseEditor                                          from '../../../BaseEditor'

class AbilityEditor extends React.Component {

	static propTypes = {
		ability : PropTypes.object.isRequired,
		onChange: PropTypes.func.isRequired,
		onDelete: PropTypes.func.isRequired
	};

	costChange = e => this.props.onChange(Object.assign({}, this.props.ability, {cost: parseInt(e.target.value, 10)}), this.props.index);

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.ability, {description: e.target.value}), this.props.index);

	nameChange = e => this.props.onChange(Object.assign({}, this.props.ability, {name: e.target.value}), this.props.index);

	onDelete = event => {
		event.preventDefault();
		this.props.onDelete(this.props.index);
	};

	render() {
		let {cost, description, name} = this.props.ability;
		return (
				<BaseEditor id={'raceEditor'} onDelete={this.onDelete}>
					<TextFormGroup id={"abilityName"}
					               label="Ability Name"
					               onChange={this.nameChange}
					               value={name}/>
					<NumberFormGroup id={"cost"}
					                 label="Cost"
					                 onChange={this.costChange}
					                 value={cost}/>
					<TextAreaFormGroup id={"abilityDescription"}
					                   label="Description"
					                   onChange={this.descriptionChange}
					                   value={description}
					/>
				</BaseEditor>
		);
	}


}

export default AbilityEditor
