import Modifier from './Modifier';

describe('Modifier', () => {
  describe('constructor', () => {
    it('should create a modifier with default values', () => {
      const modifier = new Modifier();
      expect(modifier.name).toBe('');
      expect(modifier.description).toBe('');
      expect(modifier.powerPointModifier).toBe(0);
    });

    it('should create a modifier with provided data', () => {
      const data = {
        name: 'Additional Recipient',
        description: 'Affects one additional target',
        powerPointModifier: 1
      };
      const modifier = new Modifier(data);
      expect(modifier.name).toBe('Additional Recipient');
      expect(modifier.description).toBe('Affects one additional target');
      expect(modifier.powerPointModifier).toBe(1);
    });

    it('should handle negative power point modifiers', () => {
      const modifier = new Modifier({ 
        name: 'Reduced Range',
        description: 'Halves the range',
        powerPointModifier: -1 
      });
      expect(modifier.powerPointModifier).toBe(-1);
    });
  });

  describe('isValid', () => {
    it('should return true for valid modifier', () => {
      const modifier = new Modifier({
        name: 'Test Modifier',
        description: 'Test description'
      });
      expect(modifier.isValid()).toBe(true);
    });

    it('should return false when name is empty', () => {
      const modifier = new Modifier({
        description: 'Test description'
      });
      expect(modifier.isValid()).toBe(false);
    });

    it('should return false when description is empty', () => {
      const modifier = new Modifier({
        name: 'Test Modifier'
      });
      expect(modifier.isValid()).toBe(false);
    });

    it('should return false when both name and description are empty', () => {
      const modifier = new Modifier();
      expect(modifier.isValid()).toBe(false);
    });

    it('should be valid even with powerPointModifier of 0', () => {
      const modifier = new Modifier({
        name: 'Test Modifier',
        description: 'No cost change',
        powerPointModifier: 0
      });
      expect(modifier.isValid()).toBe(true);
    });
  });

  describe('toJSON', () => {
    it('should convert modifier to JSON format', () => {
      const modifier = new Modifier({
        name: 'Heavy Armor',
        description: 'Increases armor bonus',
        powerPointModifier: 2
      });
      
      const json = modifier.toJSON();
      expect(json).toEqual({
        name: 'Heavy Armor',
        description: 'Increases armor bonus',
        powerPointModifier: 2
      });
    });

    it('should handle default values in JSON', () => {
      const modifier = new Modifier();
      const json = modifier.toJSON();
      expect(json).toEqual({
        name: '',
        description: '',
        powerPointModifier: 0
      });
    });
  });
});