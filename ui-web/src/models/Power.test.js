import Power from './Power';
import Modifier from './Modifier';

describe('Power', () => {
  describe('constructor', () => {
    it('should create a power with default values', () => {
      const power = new Power();
      expect(power.name).toBe('');
      expect(power.description).toBe('');
      expect(power.powerPoints).toBe(1);
      expect(power.range).toBe('');
      expect(power.duration).toBe('');
      expect(power.trappings).toBe('');
      expect(power.modifiers).toEqual([]);
      expect(power.rank).toBe('Novice');
    });

    it('should create a power with provided data', () => {
      const data = {
        name: 'Bolt',
        description: 'Hurls a bolt of energy',
        powerPoints: 1,
        range: 'Smarts x2',
        duration: 'Instant',
        trappings: 'Fire, Lightning, Ice',
        rank: 'Seasoned'
      };
      const power = new Power(data);
      expect(power.name).toBe('Bolt');
      expect(power.description).toBe('Hurls a bolt of energy');
      expect(power.powerPoints).toBe(1);
      expect(power.range).toBe('Smarts x2');
      expect(power.duration).toBe('Instant');
      expect(power.trappings).toBe('Fire, Lightning, Ice');
      expect(power.rank).toBe('Seasoned');
    });
  });

  describe('isValid', () => {
    it('should return true for valid power', () => {
      const power = new Power({ name: 'Bolt', powerPoints: 1 });
      expect(power.isValid()).toBe(true);
    });

    it('should return false when name is empty', () => {
      const power = new Power({ powerPoints: 1 });
      expect(power.isValid()).toBe(false);
    });

    it('should return false when powerPoints is 0', () => {
      const power = new Power({ name: 'Bolt', powerPoints: 0 });
      expect(power.isValid()).toBe(false);
    });

    it('should return false when powerPoints is negative', () => {
      const power = new Power({ name: 'Bolt', powerPoints: -1 });
      expect(power.isValid()).toBe(false);
    });
  });

  describe('addModifier', () => {
    it('should add a modifier to the power', () => {
      const power = new Power();
      const modifier = new Modifier({ name: 'Extra Target', powerPointModifier: 1 });
      power.addModifier(modifier);
      expect(power.modifiers.length).toBe(1);
      expect(power.modifiers[0]).toBe(modifier);
    });
  });

  describe('removeModifier', () => {
    it('should remove a modifier at the specified index', () => {
      const power = new Power();
      const modifier1 = new Modifier({ name: 'Modifier 1' });
      const modifier2 = new Modifier({ name: 'Modifier 2' });
      power.addModifier(modifier1);
      power.addModifier(modifier2);
      
      power.removeModifier(0);
      expect(power.modifiers.length).toBe(1);
      expect(power.modifiers[0]).toBe(modifier2);
    });

    it('should not remove anything if index is out of bounds', () => {
      const power = new Power();
      const modifier = new Modifier({ name: 'Modifier 1' });
      power.addModifier(modifier);
      
      power.removeModifier(5);
      expect(power.modifiers.length).toBe(1);
      
      power.removeModifier(-1);
      expect(power.modifiers.length).toBe(1);
    });
  });

  describe('getTotalPowerPoints', () => {
    let power;
    
    beforeEach(() => {
      power = new Power({ powerPoints: 2 });
      power.addModifier(new Modifier({ 
        name: 'Additional Target', 
        powerPointModifier: 1 
      }));
      power.addModifier(new Modifier({ 
        name: 'Increased Duration', 
        powerPointModifier: 2 
      }));
      power.addModifier(new Modifier({ 
        name: 'Reduced Cost', 
        powerPointModifier: -1 
      }));
    });

    it('should return base power points when no modifiers selected', () => {
      expect(power.getTotalPowerPoints()).toBe(2);
    });

    it('should add positive modifier costs', () => {
      expect(power.getTotalPowerPoints([0])).toBe(3); // 2 + 1
      expect(power.getTotalPowerPoints([1])).toBe(4); // 2 + 2
      expect(power.getTotalPowerPoints([0, 1])).toBe(5); // 2 + 1 + 2
    });

    it('should subtract negative modifier costs', () => {
      expect(power.getTotalPowerPoints([2])).toBe(1); // 2 - 1
    });

    it('should not go below 0', () => {
      power.powerPoints = 1;
      power.modifiers[2].powerPointModifier = -5;
      expect(power.getTotalPowerPoints([2])).toBe(0); // Max(0, 1 - 5)
    });

    it('should ignore invalid indices', () => {
      expect(power.getTotalPowerPoints([0, 10, -1])).toBe(3); // Only index 0 is valid
    });
  });

  describe('toJSON', () => {
    it('should convert power to JSON format', () => {
      const power = new Power({
        name: 'Armor',
        description: 'Grants protection',
        powerPoints: 2,
        range: 'Touch',
        duration: '5 rounds',
        trappings: 'Shimmering field',
        rank: 'Novice'
      });
      
      const modifier = new Modifier({
        name: 'Heavy Armor',
        powerPointModifier: 2
      });
      power.addModifier(modifier);
      
      const json = power.toJSON();
      expect(json).toEqual({
        name: 'Armor',
        description: 'Grants protection',
        powerPoints: 2,
        range: 'Touch',
        duration: '5 rounds',
        trappings: 'Shimmering field',
        modifiers: [modifier.toJSON()],
        rank: 'Novice'
      });
    });
  });
});