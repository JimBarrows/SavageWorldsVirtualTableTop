import Scene from './Scene'

describe('Scene', () => {
  let scene

  beforeEach(() => {
    scene = new Scene()
  })

  describe('constructor', () => {
    test('should initialize with default values', () => {
      expect(scene.id).toBe('')
      expect(scene.name).toBe('')
      expect(scene.description).toBe('')
      expect(scene.dramatis_personae).toEqual([])
      expect(scene.places).toEqual([])
    })
  })

  describe('addCharacter', () => {
    test('should add a character to dramatis personae', () => {
      const character = { name: 'Sir Gareth', description: 'A noble knight' }
      scene.addCharacter(character)
      
      expect(scene.dramatis_personae).toHaveLength(1)
      expect(scene.dramatis_personae[0]).toEqual(character)
    })

    test('should not add duplicate characters', () => {
      const character = { name: 'Sir Gareth', description: 'A noble knight' }
      scene.addCharacter(character)
      scene.addCharacter(character)
      
      expect(scene.dramatis_personae).toHaveLength(1)
    })

    test('should throw error for invalid character', () => {
      expect(() => scene.addCharacter(null)).toThrow('Character must be provided')
      expect(() => scene.addCharacter({})).toThrow('Character must have a name')
      expect(() => scene.addCharacter({ name: '' })).toThrow('Character must have a name')
    })
  })

  describe('removeCharacter', () => {
    beforeEach(() => {
      scene.addCharacter({ name: 'Sir Gareth', description: 'A noble knight' })
      scene.addCharacter({ name: 'Mara', description: 'A cunning thief' })
    })

    test('should remove a character from dramatis personae', () => {
      scene.removeCharacter('Sir Gareth')
      
      expect(scene.dramatis_personae).toHaveLength(1)
      expect(scene.dramatis_personae.find(char => char.name === 'Sir Gareth')).toBeUndefined()
    })

    test('should not fail when removing non-existent character', () => {
      expect(() => scene.removeCharacter('NonExistent')).not.toThrow()
      expect(scene.dramatis_personae).toHaveLength(2)
    })

    test('should throw error for invalid character name', () => {
      expect(() => scene.removeCharacter(null)).toThrow('Character name must be provided')
      expect(() => scene.removeCharacter('')).toThrow('Character name must be provided')
    })
  })

  describe('updateCharacter', () => {
    beforeEach(() => {
      scene.addCharacter({ name: 'Sir Gareth', description: 'A noble knight' })
    })

    test('should update character description', () => {
      scene.updateCharacter('Sir Gareth', { description: 'The party leader' })
      
      const character = scene.dramatis_personae.find(char => char.name === 'Sir Gareth')
      expect(character.description).toBe('The party leader')
    })

    test('should update character name', () => {
      scene.updateCharacter('Sir Gareth', { name: 'Sir Gareth the Bold' })
      
      expect(scene.dramatis_personae.find(char => char.name === 'Sir Gareth')).toBeUndefined()
      expect(scene.dramatis_personae.find(char => char.name === 'Sir Gareth the Bold')).toBeDefined()
    })

    test('should throw error for non-existent character', () => {
      expect(() => scene.updateCharacter('NonExistent', { description: 'Updated' }))
        .toThrow('Character NonExistent not found in dramatis personae')
    })

    test('should throw error for invalid parameters', () => {
      expect(() => scene.updateCharacter(null, {})).toThrow('Character name must be provided')
      expect(() => scene.updateCharacter('', {})).toThrow('Character name must be provided')
      expect(() => scene.updateCharacter('Sir Gareth', null)).toThrow('Updates must be provided')
    })
  })

  describe('getCharacter', () => {
    beforeEach(() => {
      scene.addCharacter({ name: 'Sir Gareth', description: 'A noble knight' })
    })

    test('should return character by name', () => {
      const character = scene.getCharacter('Sir Gareth')
      expect(character).toEqual({ name: 'Sir Gareth', description: 'A noble knight' })
    })

    test('should return undefined for non-existent character', () => {
      const character = scene.getCharacter('NonExistent')
      expect(character).toBeUndefined()
    })

    test('should throw error for invalid name', () => {
      expect(() => scene.getCharacter(null)).toThrow('Character name must be provided')
      expect(() => scene.getCharacter('')).toThrow('Character name must be provided')
    })
  })

  describe('hasCharacter', () => {
    beforeEach(() => {
      scene.addCharacter({ name: 'Sir Gareth', description: 'A noble knight' })
    })

    test('should return true for existing character', () => {
      expect(scene.hasCharacter('Sir Gareth')).toBe(true)
    })

    test('should return false for non-existent character', () => {
      expect(scene.hasCharacter('NonExistent')).toBe(false)
    })

    test('should throw error for invalid name', () => {
      expect(() => scene.hasCharacter(null)).toThrow('Character name must be provided')
      expect(() => scene.hasCharacter('')).toThrow('Character name must be provided')
    })
  })

  describe('getCharacterCount', () => {
    test('should return 0 for empty dramatis personae', () => {
      expect(scene.getCharacterCount()).toBe(0)
    })

    test('should return correct count after adding characters', () => {
      scene.addCharacter({ name: 'Sir Gareth', description: 'A noble knight' })
      scene.addCharacter({ name: 'Mara', description: 'A cunning thief' })
      expect(scene.getCharacterCount()).toBe(2)
    })

    test('should return correct count after removing characters', () => {
      scene.addCharacter({ name: 'Sir Gareth', description: 'A noble knight' })
      scene.addCharacter({ name: 'Mara', description: 'A cunning thief' })
      scene.removeCharacter('Sir Gareth')
      expect(scene.getCharacterCount()).toBe(1)
    })
  })

  describe('clearDramatisPersonae', () => {
    beforeEach(() => {
      scene.addCharacter({ name: 'Sir Gareth', description: 'A noble knight' })
      scene.addCharacter({ name: 'Mara', description: 'A cunning thief' })
    })

    test('should remove all characters from dramatis personae', () => {
      scene.clearDramatisPersonae()
      expect(scene.dramatis_personae).toHaveLength(0)
      expect(scene.getCharacterCount()).toBe(0)
    })
  })

  describe('addPlace', () => {
    test('should add a place to scene places', () => {
      const place = { name: 'Red Dragon Inn', description: 'A cozy tavern' }
      scene.addPlace(place)
      
      expect(scene.places).toHaveLength(1)
      expect(scene.places[0]).toEqual(place)
    })

    test('should not add duplicate places', () => {
      const place = { name: 'Red Dragon Inn', description: 'A cozy tavern' }
      scene.addPlace(place)
      scene.addPlace(place)
      
      expect(scene.places).toHaveLength(1)
    })

    test('should throw error for invalid place', () => {
      expect(() => scene.addPlace(null)).toThrow('Place must be provided')
      expect(() => scene.addPlace({})).toThrow('Place must have a name')
      expect(() => scene.addPlace({ name: '' })).toThrow('Place must have a name')
    })
  })

  describe('removePlace', () => {
    beforeEach(() => {
      scene.addPlace({ name: 'Red Dragon Inn', description: 'A cozy tavern' })
      scene.addPlace({ name: 'Main Hall', description: 'The central dining area' })
    })

    test('should remove a place from scene places', () => {
      scene.removePlace('Red Dragon Inn')
      
      expect(scene.places).toHaveLength(1)
      expect(scene.places.find(place => place.name === 'Red Dragon Inn')).toBeUndefined()
    })

    test('should not fail when removing non-existent place', () => {
      expect(() => scene.removePlace('NonExistent')).not.toThrow()
      expect(scene.places).toHaveLength(2)
    })

    test('should throw error for invalid place name', () => {
      expect(() => scene.removePlace(null)).toThrow('Place name must be provided')
      expect(() => scene.removePlace('')).toThrow('Place name must be provided')
    })
  })

  describe('updatePlace', () => {
    beforeEach(() => {
      scene.addPlace({ name: 'Red Dragon Inn', description: 'A cozy tavern' })
    })

    test('should update place description', () => {
      scene.updatePlace('Red Dragon Inn', { description: 'A bustling tavern' })
      
      const place = scene.places.find(p => p.name === 'Red Dragon Inn')
      expect(place.description).toBe('A bustling tavern')
    })

    test('should update place name', () => {
      scene.updatePlace('Red Dragon Inn', { name: 'The Red Dragon Inn' })
      
      expect(scene.places.find(p => p.name === 'Red Dragon Inn')).toBeUndefined()
      expect(scene.places.find(p => p.name === 'The Red Dragon Inn')).toBeDefined()
    })

    test('should throw error for non-existent place', () => {
      expect(() => scene.updatePlace('NonExistent', { description: 'Updated' }))
        .toThrow('Place NonExistent not found in scene places')
    })

    test('should throw error for invalid parameters', () => {
      expect(() => scene.updatePlace(null, {})).toThrow('Place name must be provided')
      expect(() => scene.updatePlace('', {})).toThrow('Place name must be provided')
      expect(() => scene.updatePlace('Red Dragon Inn', null)).toThrow('Updates must be provided')
    })
  })

  describe('getPlace', () => {
    beforeEach(() => {
      scene.addPlace({ name: 'Red Dragon Inn', description: 'A cozy tavern' })
    })

    test('should return place by name', () => {
      const place = scene.getPlace('Red Dragon Inn')
      expect(place).toEqual({ name: 'Red Dragon Inn', description: 'A cozy tavern' })
    })

    test('should return undefined for non-existent place', () => {
      const place = scene.getPlace('NonExistent')
      expect(place).toBeUndefined()
    })

    test('should throw error for invalid name', () => {
      expect(() => scene.getPlace(null)).toThrow('Place name must be provided')
      expect(() => scene.getPlace('')).toThrow('Place name must be provided')
    })
  })

  describe('hasPlace', () => {
    beforeEach(() => {
      scene.addPlace({ name: 'Red Dragon Inn', description: 'A cozy tavern' })
    })

    test('should return true for existing place', () => {
      expect(scene.hasPlace('Red Dragon Inn')).toBe(true)
    })

    test('should return false for non-existent place', () => {
      expect(scene.hasPlace('NonExistent')).toBe(false)
    })

    test('should throw error for invalid name', () => {
      expect(() => scene.hasPlace(null)).toThrow('Place name must be provided')
      expect(() => scene.hasPlace('')).toThrow('Place name must be provided')
    })
  })

  describe('getPlaceCount', () => {
    test('should return 0 for empty places', () => {
      expect(scene.getPlaceCount()).toBe(0)
    })

    test('should return correct count after adding places', () => {
      scene.addPlace({ name: 'Red Dragon Inn', description: 'A cozy tavern' })
      scene.addPlace({ name: 'Main Hall', description: 'The central dining area' })
      expect(scene.getPlaceCount()).toBe(2)
    })

    test('should return correct count after removing places', () => {
      scene.addPlace({ name: 'Red Dragon Inn', description: 'A cozy tavern' })
      scene.addPlace({ name: 'Main Hall', description: 'The central dining area' })
      scene.removePlace('Red Dragon Inn')
      expect(scene.getPlaceCount()).toBe(1)
    })
  })

  describe('clearPlaces', () => {
    beforeEach(() => {
      scene.addPlace({ name: 'Red Dragon Inn', description: 'A cozy tavern' })
      scene.addPlace({ name: 'Main Hall', description: 'The central dining area' })
    })

    test('should remove all places from scene places', () => {
      scene.clearPlaces()
      expect(scene.places).toHaveLength(0)
      expect(scene.getPlaceCount()).toBe(0)
    })
  })
})