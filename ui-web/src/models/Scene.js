export default class Scene {
	constructor() {
		this.id = ''
		this.name = ''
		this.description = ''
		this.dramatis_personae = []
		this.places = []
	}

	addCharacter(character) {
		if (!character) {
			throw new Error('Character must be provided')
		}
		if (!character.name || character.name.trim() === '') {
			throw new Error('Character must have a name')
		}

		// Check for duplicate characters
		if (this.dramatis_personae.find(char => char.name === character.name)) {
			return // Don't add duplicates
		}

		this.dramatis_personae.push({ ...character })
	}

	removeCharacter(characterName) {
		if (!characterName || characterName.trim() === '') {
			throw new Error('Character name must be provided')
		}

		this.dramatis_personae = this.dramatis_personae.filter(
			char => char.name !== characterName
		)
	}

	updateCharacter(characterName, updates) {
		if (!characterName || characterName.trim() === '') {
			throw new Error('Character name must be provided')
		}
		if (!updates) {
			throw new Error('Updates must be provided')
		}

		const characterIndex = this.dramatis_personae.findIndex(
			char => char.name === characterName
		)
		
		if (characterIndex === -1) {
			throw new Error(`Character ${characterName} not found in dramatis personae`)
		}

		// Update the character
		this.dramatis_personae[characterIndex] = {
			...this.dramatis_personae[characterIndex],
			...updates
		}
	}

	getCharacter(characterName) {
		if (!characterName || characterName.trim() === '') {
			throw new Error('Character name must be provided')
		}

		return this.dramatis_personae.find(char => char.name === characterName)
	}

	hasCharacter(characterName) {
		if (!characterName || characterName.trim() === '') {
			throw new Error('Character name must be provided')
		}

		return this.dramatis_personae.some(char => char.name === characterName)
	}

	getCharacterCount() {
		return this.dramatis_personae.length
	}

	clearDramatisPersonae() {
		this.dramatis_personae = []
	}

	addPlace(place) {
		if (!place) {
			throw new Error('Place must be provided')
		}
		if (!place.name || place.name.trim() === '') {
			throw new Error('Place must have a name')
		}

		// Check for duplicate places
		if (this.places.find(p => p.name === place.name)) {
			return // Don't add duplicates
		}

		this.places.push({ ...place })
	}

	removePlace(placeName) {
		if (!placeName || placeName.trim() === '') {
			throw new Error('Place name must be provided')
		}

		this.places = this.places.filter(
			place => place.name !== placeName
		)
	}

	updatePlace(placeName, updates) {
		if (!placeName || placeName.trim() === '') {
			throw new Error('Place name must be provided')
		}
		if (!updates) {
			throw new Error('Updates must be provided')
		}

		const placeIndex = this.places.findIndex(
			place => place.name === placeName
		)
		
		if (placeIndex === -1) {
			throw new Error(`Place ${placeName} not found in scene places`)
		}

		// Update the place
		this.places[placeIndex] = {
			...this.places[placeIndex],
			...updates
		}
	}

	getPlace(placeName) {
		if (!placeName || placeName.trim() === '') {
			throw new Error('Place name must be provided')
		}

		return this.places.find(place => place.name === placeName)
	}

	hasPlace(placeName) {
		if (!placeName || placeName.trim() === '') {
			throw new Error('Place name must be provided')
		}

		return this.places.some(place => place.name === placeName)
	}

	getPlaceCount() {
		return this.places.length
	}

	clearPlaces() {
		this.places = []
	}
}