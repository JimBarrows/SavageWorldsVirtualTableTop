import React, { Component } from 'react'
import * as PropTypes from 'prop-types'
import Scene from '../../propTypes/Scene'

export default class SceneEditor extends Component {
	static propTypes = {
		scene: Scene,
		availableCharacters: PropTypes.arrayOf(PropTypes.shape({
			name: PropTypes.string.isRequired,
			description: PropTypes.string
		})),
		availablePlaces: PropTypes.arrayOf(PropTypes.shape({
			name: PropTypes.string.isRequired,
			description: PropTypes.string
		})),
		onChange: PropTypes.func,
		onSave: PropTypes.func
	}

	static defaultProps = {
		scene: {
			id: '',
			name: '',
			description: '',
			dramatis_personae: [],
			places: []
		},
		availableCharacters: [],
		availablePlaces: []
	}

	constructor(props) {
		super(props)
		this.state = {
			scene: props.scene || {
				id: '',
				name: '',
				description: '',
				dramatis_personae: [],
				places: []
			},
			selectedCharacter: '',
			editingCharacter: null,
			selectedPlace: '',
			editingPlace: null,
			errors: {},
			showCharacterAlreadyInSceneError: false,
			showPlaceAlreadyInSceneError: false
		}
	}

	componentDidUpdate(prevProps) {
		if (prevProps.scene !== this.props.scene && this.props.scene) {
			this.setState({ scene: this.props.scene })
		}
	}

	handleFieldChange = (event) => {
		const { name, value } = event.target
		const updatedScene = {
			...this.state.scene,
			[name]: value
		}
		
		this.setState({ scene: updatedScene })
		
		if (this.props.onChange) {
			this.props.onChange(updatedScene)
		}
	}

	handleAddCharacter = () => {
		const { selectedCharacter } = this.state
		const { availableCharacters } = this.props
		
		if (!selectedCharacter) return

		// Check if character is already in scene
		const isAlreadyInScene = this.state.scene.dramatis_personae.some(
			char => char.name === selectedCharacter
		)
		
		if (isAlreadyInScene) {
			this.setState({ showCharacterAlreadyInSceneError: true })
			return
		}

		// Find character details
		const characterDetails = availableCharacters.find(
			char => char.name === selectedCharacter
		) || { name: selectedCharacter, description: '' }

		const updatedScene = {
			...this.state.scene,
			dramatis_personae: [
				...this.state.scene.dramatis_personae,
				characterDetails
			]
		}

		this.setState({ 
			scene: updatedScene, 
			selectedCharacter: '',
			showCharacterAlreadyInSceneError: false
		})
		
		if (this.props.onChange) {
			this.props.onChange(updatedScene)
		}
	}

	handleRemoveCharacter = (characterName) => {
		const updatedScene = {
			...this.state.scene,
			dramatis_personae: this.state.scene.dramatis_personae.filter(
				char => char.name !== characterName
			)
		}

		this.setState({ scene: updatedScene })
		
		if (this.props.onChange) {
			this.props.onChange(updatedScene)
		}
	}

	handleEditCharacter = (character) => {
		this.setState({ editingCharacter: { ...character } })
	}

	handleSaveCharacter = () => {
		const { editingCharacter } = this.state
		if (!editingCharacter) return

		const updatedScene = {
			...this.state.scene,
			dramatis_personae: this.state.scene.dramatis_personae.map(char =>
				char.name === editingCharacter.name ? editingCharacter : char
			)
		}

		this.setState({ 
			scene: updatedScene,
			editingCharacter: null
		})
		
		if (this.props.onChange) {
			this.props.onChange(updatedScene)
		}
	}

	handleCharacterDescriptionChange = (event) => {
		const { value } = event.target
		this.setState({
			editingCharacter: {
				...this.state.editingCharacter,
				description: value
			}
		})
	}

	// Place management methods
	handleAddPlace = () => {
		const { selectedPlace } = this.state
		const { availablePlaces } = this.props
		
		if (!selectedPlace) return

		// Check if place is already in scene
		const isAlreadyInScene = this.state.scene.places.some(
			place => place.name === selectedPlace
		)
		
		if (isAlreadyInScene) {
			this.setState({ showPlaceAlreadyInSceneError: true })
			return
		}

		// Find place details
		const placeDetails = availablePlaces.find(
			place => place.name === selectedPlace
		) || { name: selectedPlace, description: '' }

		const updatedScene = {
			...this.state.scene,
			places: [
				...this.state.scene.places,
				placeDetails
			]
		}

		this.setState({ 
			scene: updatedScene, 
			selectedPlace: '',
			showPlaceAlreadyInSceneError: false
		})
		
		if (this.props.onChange) {
			this.props.onChange(updatedScene)
		}
	}

	handleRemovePlace = (placeName) => {
		const updatedScene = {
			...this.state.scene,
			places: this.state.scene.places.filter(
				place => place.name !== placeName
			)
		}

		this.setState({ scene: updatedScene })
		
		if (this.props.onChange) {
			this.props.onChange(updatedScene)
		}
	}

	handleEditPlace = (place) => {
		this.setState({ editingPlace: { ...place } })
	}

	handleSavePlace = () => {
		const { editingPlace } = this.state
		if (!editingPlace) return

		const updatedScene = {
			...this.state.scene,
			places: this.state.scene.places.map(place =>
				place.name === editingPlace.name ? editingPlace : place
			)
		}

		this.setState({ 
			scene: updatedScene,
			editingPlace: null
		})
		
		if (this.props.onChange) {
			this.props.onChange(updatedScene)
		}
	}

	handlePlaceDescriptionChange = (event) => {
		const { value } = event.target
		this.setState({
			editingPlace: {
				...this.state.editingPlace,
				description: value
			}
		})
	}

	handleSave = () => {
		const { scene } = this.state
		const errors = {}

		// Validation
		if (!scene.name || scene.name.trim() === '') {
			errors.name = 'Scene name is required'
		}

		if (Object.keys(errors).length > 0) {
			this.setState({ errors })
			return
		}

		this.setState({ errors: {} })

		if (this.props.onSave) {
			this.props.onSave(scene)
		}
	}

	render() {
		const { scene, selectedCharacter, editingCharacter, selectedPlace, editingPlace, errors, showCharacterAlreadyInSceneError, showPlaceAlreadyInSceneError } = this.state
		const { availableCharacters, availablePlaces } = this.props

		return (
			<div className="scene-editor">
				<div className="form-group">
					<label htmlFor="scene-name">Scene Name</label>
					<input
						type="text"
						id="scene-name"
						name="name"
						className="form-control"
						placeholder="Scene Name"
						value={scene.name}
						onChange={this.handleFieldChange}
					/>
					{errors.name && <div className="text-danger">{errors.name}</div>}
				</div>

				<div className="form-group">
					<label htmlFor="scene-description">Scene Description</label>
					<textarea
						id="scene-description"
						name="description"
						className="form-control"
						placeholder="Scene Description"
						rows="3"
						value={scene.description}
						onChange={this.handleFieldChange}
					/>
				</div>

				<div className="form-group">
					<h4>Dramatis Personae</h4>
					
					{scene.dramatis_personae.length > 0 ? (
						<table className="table">
							<thead>
								<tr>
									<th>Name</th>
									<th>Description</th>
									<th>Actions</th>
								</tr>
							</thead>
							<tbody>
								{scene.dramatis_personae.map((character, index) => (
									<tr key={index} className="dramatis-personae-character-row">
										<td>{character.name}</td>
										<td className="character-description">
											{editingCharacter && editingCharacter.name === character.name ? (
												<input
													type="text"
													value={editingCharacter.description}
													onChange={this.handleCharacterDescriptionChange}
												/>
											) : (
												character.description
											)}
										</td>
										<td>
											{editingCharacter && editingCharacter.name === character.name ? (
												<button
													type="button"
													className="btn btn-success btn-sm me-2"
													onClick={this.handleSaveCharacter}
												>
													Save Character
												</button>
											) : (
												<>
													<button
														type="button"
														className="btn btn-sm btn-secondary me-2 edit-character-button"
														onClick={() => this.handleEditCharacter(character)}
													>
														Edit
													</button>
													<button
														type="button"
														className="btn btn-sm btn-danger remove-character-button"
														onClick={() => this.handleRemoveCharacter(character.name)}
													>
														Remove
													</button>
												</>
											)}
										</td>
									</tr>
								))}
							</tbody>
						</table>
					) : (
						<p>No characters in this scene yet.</p>
					)}

					<div className="add-character-section">
						<div className="row">
							<div className="col-md-8">
								<select
									className="form-control"
									role="combobox"
									value={selectedCharacter}
									onChange={(e) => this.setState({ selectedCharacter: e.target.value })}
								>
									<option value="">Select a character...</option>
									{availableCharacters.map((character, index) => (
										<option key={index} value={character.name}>
											{character.name}
										</option>
									))}
								</select>
							</div>
							<div className="col-md-4">
								<button
									type="button"
									className="btn btn-primary"
									onClick={this.handleAddCharacter}
									disabled={!selectedCharacter}
								>
									Add Character
								</button>
							</div>
						</div>
						{showCharacterAlreadyInSceneError && (
							<div className="text-danger mt-2">
								Character is already in the scene
							</div>
						)}
					</div>
				</div>

				<div className="form-group">
					<h4>Scene Places</h4>
					
					{scene.places && scene.places.length > 0 ? (
						<table className="table">
							<thead>
								<tr>
									<th>Name</th>
									<th>Description</th>
									<th>Actions</th>
								</tr>
							</thead>
							<tbody>
								{scene.places.map((place, index) => (
									<tr key={index} className="scene-places-place-row">
										<td>{place.name}</td>
										<td className="place-description">
											{editingPlace && editingPlace.name === place.name ? (
												<input
													type="text"
													value={editingPlace.description}
													onChange={this.handlePlaceDescriptionChange}
												/>
											) : (
												place.description
											)}
										</td>
										<td>
											{editingPlace && editingPlace.name === place.name ? (
												<button
													type="button"
													className="btn btn-success btn-sm me-2"
													onClick={this.handleSavePlace}
												>
													Save Place
												</button>
											) : (
												<>
													<button
														type="button"
														className="btn btn-sm btn-secondary me-2 edit-place-button"
														onClick={() => this.handleEditPlace(place)}
													>
														Edit
													</button>
													<button
														type="button"
														className="btn btn-sm btn-danger remove-place-button"
														onClick={() => this.handleRemovePlace(place.name)}
													>
														Remove
													</button>
												</>
											)}
										</td>
									</tr>
								))}
							</tbody>
						</table>
					) : (
						<p>No places in this scene yet.</p>
					)}

					<div className="add-place-section">
						<div className="row">
							<div className="col-md-8">
								<select
									className="form-control"
									role="combobox"
									value={selectedPlace}
									onChange={(e) => this.setState({ selectedPlace: e.target.value })}
									id="select-place-for-scene"
								>
									<option value="">Select a place...</option>
									{availablePlaces.map((place, index) => (
										<option key={index} value={place.name}>
											{place.name}
										</option>
									))}
								</select>
							</div>
							<div className="col-md-4">
								<button
									type="button"
									className="btn btn-primary"
									onClick={this.handleAddPlace}
									disabled={!selectedPlace}
									id="button-add-place-to-scene"
								>
									Add Place
								</button>
							</div>
						</div>
						{showPlaceAlreadyInSceneError && (
							<div className="text-danger mt-2">
								Place is already in the scene
							</div>
						)}
					</div>
				</div>

				<div className="form-actions">
					<button
						type="button"
						className="btn btn-success"
						onClick={this.handleSave}
					>
						Save Scene
					</button>
				</div>
			</div>
		)
	}
}