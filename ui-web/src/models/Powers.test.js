import Powers from './Powers';
import Power from './Power';

describe('Powers', () => {
  describe('constructor', () => {
    it('should create Powers with empty arrays by default', () => {
      const powers = new Powers();
      expect(powers.arcaneBackgrounds).toEqual([]);
      expect(powers.powers).toEqual([]);
    });

    it('should create Powers with provided data', () => {
      const power1 = new Power({ name: 'Bolt' });
      const power2 = new Power({ name: 'Healing' });
      const data = {
        arcaneBackgrounds: ['Miracles', 'Magic'],
        powers: [power1, power2]
      };
      const powers = new Powers(data);
      expect(powers.arcaneBackgrounds).toEqual(['Miracles', 'Magic']);
      expect(powers.powers).toEqual([power1, power2]);
    });
  });

  describe('addPower', () => {
    it('should add a power to the list', () => {
      const powers = new Powers();
      const power = new Power({ name: 'Bolt' });
      powers.addPower(power);
      expect(powers.powers.length).toBe(1);
      expect(powers.powers[0]).toBe(power);
    });
  });

  describe('removePower', () => {
    it('should remove a power at the specified index', () => {
      const powers = new Powers();
      const power1 = new Power({ name: 'Bolt' });
      const power2 = new Power({ name: 'Healing' });
      powers.addPower(power1);
      powers.addPower(power2);
      
      powers.removePower(0);
      expect(powers.powers.length).toBe(1);
      expect(powers.powers[0]).toBe(power2);
    });

    it('should not remove anything if index is out of bounds', () => {
      const powers = new Powers();
      const power = new Power({ name: 'Bolt' });
      powers.addPower(power);
      
      powers.removePower(5);
      expect(powers.powers.length).toBe(1);
      
      powers.removePower(-1);
      expect(powers.powers.length).toBe(1);
    });
  });

  describe('getDefaultPowers', () => {
    it('should return an array of default powers', () => {
      const defaultPowers = Powers.getDefaultPowers();
      expect(defaultPowers).toBeInstanceOf(Array);
      expect(defaultPowers.length).toBe(5);
    });

    it('should include Arcane Protection', () => {
      const defaultPowers = Powers.getDefaultPowers();
      const arcaneProtection = defaultPowers.find(p => p.name === 'Arcane Protection');
      expect(arcaneProtection).toBeDefined();
      expect(arcaneProtection.powerPoints).toBe(1);
      expect(arcaneProtection.range).toBe('Touch');
      expect(arcaneProtection.duration).toBe('5 rounds');
    });

    it('should include Bolt', () => {
      const defaultPowers = Powers.getDefaultPowers();
      const bolt = defaultPowers.find(p => p.name === 'Bolt');
      expect(bolt).toBeDefined();
      expect(bolt.powerPoints).toBe(1);
      expect(bolt.range).toBe('Smarts x2');
      expect(bolt.duration).toBe('Instant');
    });

    it('should include Boost/Lower Trait', () => {
      const defaultPowers = Powers.getDefaultPowers();
      const boostLower = defaultPowers.find(p => p.name === 'Boost/Lower Trait');
      expect(boostLower).toBeDefined();
      expect(boostLower.powerPoints).toBe(2);
      expect(boostLower.range).toBe('Smarts');
      expect(boostLower.duration).toBe('5 rounds');
    });

    it('should include Detect/Conceal Arcana', () => {
      const defaultPowers = Powers.getDefaultPowers();
      const detectConceal = defaultPowers.find(p => p.name === 'Detect/Conceal Arcana');
      expect(detectConceal).toBeDefined();
      expect(detectConceal.powerPoints).toBe(2);
      expect(detectConceal.range).toBe('Sight');
      expect(detectConceal.duration).toBe('5 rounds');
    });

    it('should include Healing', () => {
      const defaultPowers = Powers.getDefaultPowers();
      const healing = defaultPowers.find(p => p.name === 'Healing');
      expect(healing).toBeDefined();
      expect(healing.powerPoints).toBe(3);
      expect(healing.range).toBe('Touch');
      expect(healing.duration).toBe('Instant');
    });

    it('should return Power instances', () => {
      const defaultPowers = Powers.getDefaultPowers();
      defaultPowers.forEach(power => {
        expect(power).toBeInstanceOf(Power);
      });
    });
  });

  describe('toJSON', () => {
    it('should convert Powers to JSON format', () => {
      const powers = new Powers({
        arcaneBackgrounds: ['Miracles', 'Psionics']
      });
      
      const power1 = new Power({ name: 'Bolt', powerPoints: 1 });
      const power2 = new Power({ name: 'Armor', powerPoints: 2 });
      powers.addPower(power1);
      powers.addPower(power2);
      
      const json = powers.toJSON();
      expect(json).toEqual({
        arcaneBackgrounds: ['Miracles', 'Psionics'],
        powers: [power1.toJSON(), power2.toJSON()]
      });
    });

    it('should handle empty arrays', () => {
      const powers = new Powers();
      const json = powers.toJSON();
      expect(json).toEqual({
        arcaneBackgrounds: [],
        powers: []
      });
    });
  });
});