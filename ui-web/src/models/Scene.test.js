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
})