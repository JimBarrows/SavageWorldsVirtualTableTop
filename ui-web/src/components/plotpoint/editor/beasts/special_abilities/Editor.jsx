import {TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes                          from 'prop-types'
import React                              from 'react'
import BaseEditor                         from '../../../../BaseEditor'

export default class Editor extends React.Component {

	static defaultProps = {}

	static propTypes = {
		onDelete         : PropTypes.func.isRequired,
		description      : PropTypes.string.isRequired,
		descriptionChange: PropTypes.func.isRequired,
		id               : PropTypes.string.isRequired,
		name             : PropTypes.string.isRequired,
		nameChange       : PropTypes.func.isRequired
	}

	onDelete = event => {
		event.preventDefault()
		let {name, description} = this.props
		this.props.onDelete({name, description})
	}

	render() {
		let {description, descriptionChange, id, name, nameChange} = this.props
		let component_id                                           = `SpecialAbilityEditor-${id}`

		return <BaseEditor id={component_id} onDelete={this.onDelete}>
			<TextFormGroup id={component_id + `-Name`} label={'Name'} onChange={nameChange}
			               required={true} value={name}/>
			<TextAreaFormGroup id={component_id + `-Description`} label={'Description'}
			                   onChange={descriptionChange} value={description}/>
		</BaseEditor>
	}
}

