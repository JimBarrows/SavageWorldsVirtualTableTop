import Character from './Character';
import Attribute from './Attribute';

describe('Character Model', () => {
  describe('Constructor', () => {
    it('creates a character with default values', () => {
      const character = new Character();
      
      expect(character.name).toBe(' ');
      expect(character.background).toBe('');
      expect(character.description).toBe('');
      expect(character.pace).toBe(6);
      expect(character.armor).toBe(0);
      expect(character.charisma).toBe(0);
      expect(character.animalIntelligence).toBe(false);
      expect(character.skills).toEqual([]);
      expect(character.edges).toEqual([]);
      expect(character.hindrances).toEqual([]);
    });

    it('initializes attributes as Attribute objects', () => {
      const character = new Character();
      
      expect(character.agility).toBeInstanceOf(Attribute);
      expect(character.smarts).toBeInstanceOf(Attribute);
      expect(character.spirit).toBeInstanceOf(Attribute);
      expect(character.strength).toBeInstanceOf(Attribute);
      expect(character.vigor).toBeInstanceOf(Attribute);
    });

    it('allows property assignment after creation', () => {
      const character = new Character();
      
      character.name = 'Test Hero';
      character.background = 'Noble';
      character.description = 'A brave warrior';
      character.pace = 8;
      character.armor = 2;
      character.charisma = 1;
      
      expect(character.name).toBe('Test Hero');
      expect(character.background).toBe('Noble');
      expect(character.description).toBe('A brave warrior');
      expect(character.pace).toBe(8);
      expect(character.armor).toBe(2);
      expect(character.charisma).toBe(1);
    });
  });

  describe('Attributes', () => {
    it('has all required attribute properties', () => {
      const character = new Character();
      
      expect(character).toHaveProperty('agility');
      expect(character).toHaveProperty('smarts');
      expect(character).toHaveProperty('spirit');
      expect(character).toHaveProperty('strength');
      expect(character).toHaveProperty('vigor');
    });

    it('allows attribute modification', () => {
      const character = new Character();
      const newAttribute = new Attribute();
      
      character.agility = newAttribute;
      expect(character.agility).toBe(newAttribute);
    });
  });

  describe('Skills', () => {
    it('initializes with empty skills array', () => {
      const character = new Character();
      
      expect(character.skills).toEqual([]);
      expect(Array.isArray(character.skills)).toBe(true);
    });

    it('allows skill addition', () => {
      const character = new Character();
      const skill = { name: 'Fighting', die: 'd8' };
      
      character.skills.push(skill);
      
      expect(character.skills).toHaveLength(1);
      expect(character.skills[0]).toEqual(skill);
    });

    it('allows multiple skills', () => {
      const character = new Character();
      const skills = [
        { name: 'Fighting', die: 'd8' },
        { name: 'Shooting', die: 'd6' },
        { name: 'Notice', die: 'd6' }
      ];
      
      character.skills = skills;
      
      expect(character.skills).toHaveLength(3);
      expect(character.skills).toEqual(skills);
    });
  });

  describe('Edges and Hindrances', () => {
    it('initializes with empty edges array', () => {
      const character = new Character();
      
      expect(character.edges).toEqual([]);
      expect(Array.isArray(character.edges)).toBe(true);
    });

    it('initializes with empty hindrances array', () => {
      const character = new Character();
      
      expect(character.hindrances).toEqual([]);
      expect(Array.isArray(character.hindrances)).toBe(true);
    });

    it('allows edge addition', () => {
      const character = new Character();
      const edge = { name: 'Quick', description: 'Act quickly in combat' };
      
      character.edges.push(edge);
      
      expect(character.edges).toHaveLength(1);
      expect(character.edges[0]).toEqual(edge);
    });

    it('allows hindrance addition', () => {
      const character = new Character();
      const hindrance = { name: 'Cautious', description: 'Slow to act' };
      
      character.hindrances.push(hindrance);
      
      expect(character.hindrances).toHaveLength(1);
      expect(character.hindrances[0]).toEqual(hindrance);
    });
  });

  describe('Character Properties', () => {
    it('has correct default pace value', () => {
      const character = new Character();
      
      expect(character.pace).toBe(6);
      expect(typeof character.pace).toBe('number');
    });

    it('has correct default armor value', () => {
      const character = new Character();
      
      expect(character.armor).toBe(0);
      expect(typeof character.armor).toBe('number');
    });

    it('has correct default charisma value', () => {
      const character = new Character();
      
      expect(character.charisma).toBe(0);
      expect(typeof character.charisma).toBe('number');
    });

    it('has correct default animal intelligence flag', () => {
      const character = new Character();
      
      expect(character.animalIntelligence).toBe(false);
      expect(typeof character.animalIntelligence).toBe('boolean');
    });

    it('has default name with space character', () => {
      const character = new Character();
      
      expect(character.name).toBe(' ');
      expect(typeof character.name).toBe('string');
    });

    it('has empty string defaults for text fields', () => {
      const character = new Character();
      
      expect(character.background).toBe('');
      expect(character.description).toBe('');
      expect(typeof character.background).toBe('string');
      expect(typeof character.description).toBe('string');
    });
  });

  describe('Property Assignment', () => {
    it('allows all properties to be modified', () => {
      const character = new Character();
      
      // Test all property assignments
      character.name = 'Hero Name';
      character.background = 'Noble Background';
      character.description = 'Detailed description';
      character.pace = 10;
      character.armor = 5;
      character.charisma = 2;
      character.animalIntelligence = true;
      character.skills = [{ name: 'Test', die: 'd6' }];
      character.edges = [{ name: 'TestEdge' }];
      character.hindrances = [{ name: 'TestHindrance' }];
      
      // Verify all assignments
      expect(character.name).toBe('Hero Name');
      expect(character.background).toBe('Noble Background');
      expect(character.description).toBe('Detailed description');
      expect(character.pace).toBe(10);
      expect(character.armor).toBe(5);
      expect(character.charisma).toBe(2);
      expect(character.animalIntelligence).toBe(true);
      expect(character.skills).toEqual([{ name: 'Test', die: 'd6' }]);
      expect(character.edges).toEqual([{ name: 'TestEdge' }]);
      expect(character.hindrances).toEqual([{ name: 'TestHindrance' }]);
    });
  });
});