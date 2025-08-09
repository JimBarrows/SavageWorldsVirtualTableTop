import Skill from './Skill';

describe('Skill', () => {
  describe('constructor', () => {
    it('should initialize with default values when no data provided', () => {
      const skill = new Skill();
      expect(skill.name).toBe('');
      expect(skill.attribute).toBe('Agility');
      expect(skill.description).toBe('');
    });

    it('should initialize with provided data', () => {
      const skillData = {
        name: 'Athletics',
        attribute: 'Agility',
        description: 'Covers running, jumping, swimming, throwing, and catching'
      };
      
      const skill = new Skill(skillData);
      expect(skill.name).toBe('Athletics');
      expect(skill.attribute).toBe('Agility');
      expect(skill.description).toBe('Covers running, jumping, swimming, throwing, and catching');
    });

    it('should use default attribute if not provided', () => {
      const skill = new Skill({ name: 'Test Skill' });
      expect(skill.attribute).toBe('Agility');
    });

    it('should accept all valid attributes', () => {
      const attributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];
      
      attributes.forEach(attr => {
        const skill = new Skill({ attribute: attr });
        expect(skill.attribute).toBe(attr);
      });
    });
  });

  describe('toJSON', () => {
    it('should return a JSON representation of the skill', () => {
      const skill = new Skill({
        name: 'Notice',
        attribute: 'Smarts',
        description: 'General awareness and perception'
      });
      
      const json = skill.toJSON();
      expect(json).toEqual({
        name: 'Notice',
        attribute: 'Smarts',
        description: 'General awareness and perception'
      });
    });

    it('should return all properties even with default values', () => {
      const skill = new Skill();
      const json = skill.toJSON();
      
      expect(json).toHaveProperty('name');
      expect(json).toHaveProperty('attribute');
      expect(json).toHaveProperty('description');
    });
  });

  describe('validation', () => {
    it('should validate that attribute is one of the allowed values', () => {
      const validAttributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];
      
      const skill = new Skill();
      expect(skill.isValidAttribute('Agility')).toBe(true);
      expect(skill.isValidAttribute('Invalid')).toBe(false);
      
      validAttributes.forEach(attr => {
        expect(skill.isValidAttribute(attr)).toBe(true);
      });
    });
  });

  describe('updateAttribute', () => {
    it('should update attribute if valid', () => {
      const skill = new Skill({ attribute: 'Agility' });
      
      skill.updateAttribute('Smarts');
      expect(skill.attribute).toBe('Smarts');
    });

    it('should not update attribute if invalid', () => {
      const skill = new Skill({ attribute: 'Agility' });
      
      skill.updateAttribute('InvalidAttribute');
      expect(skill.attribute).toBe('Agility');
    });
  });

  describe('clone', () => {
    it('should create a deep copy of the skill', () => {
      const original = new Skill({
        name: 'Fighting',
        attribute: 'Agility',
        description: 'Hand-to-hand combat'
      });
      
      const clone = original.clone();
      
      expect(clone).not.toBe(original);
      expect(clone.name).toBe(original.name);
      expect(clone.attribute).toBe(original.attribute);
      expect(clone.description).toBe(original.description);
      
      // Verify it's a deep copy
      clone.name = 'Shooting';
      expect(original.name).toBe('Fighting');
    });
  });
});