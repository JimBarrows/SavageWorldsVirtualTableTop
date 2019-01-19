import TextAreaFormGroup from 'bootstrap-react-components/distribution/formgroups/TextAreaFormGroup'
import TextFormGroup     from 'bootstrap-react-components/distribution/formgroups/TextFormGroup'
import React             from 'react'
import BaseEditor        from './BaseEditor'

export default class EffectsEditor extends React.Component {

	descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index)
	nameChange        = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index)
	onDelete          = event => {
		event.preventDefault()
		this.props.onDelete(this.props.index)
	}

	render() {
		return (
			<BaseEditor id={this.props.id} onDelete={this.onDelete} >
				<TextFormGroup id='arcaneBackgroundName' label='Name' onChange={this.nameChange} required={true}
					value={this.props.item.name} />
				<TextAreaFormGroup id={"arcaneBackgroundDescription"}
					label="Description"
					onChange={this.descriptionChange}
					value={this.props.item.description} />
			</BaseEditor >
		)
	}
}
