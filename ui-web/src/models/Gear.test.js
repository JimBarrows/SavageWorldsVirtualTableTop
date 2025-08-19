import Gear from './Gear';

describe('Gear Model', () => {
  describe('Constructor', () => {
    it('should create a new Gear instance with all properties initialized as empty arrays', () => {
      const gear = new Gear();
      
      expect(gear.aircraft).toEqual([]);
      expect(gear.ammunition).toEqual([]);
      expect(gear.armor).toEqual([]);
      expect(gear.groundVehicles).toEqual([]);
      expect(gear.handWeapons).toEqual([]);
      expect(gear.mundaneItems).toEqual([]);
      expect(gear.rangedWeapons).toEqual([]);
      expect(gear.skills).toEqual([]);
      expect(gear.specialWeapons).toEqual([]);
      expect(gear.trappingsAndEffects).toEqual([]);
      expect(gear.vehicleMountedAndAtGuns).toEqual([]);
      expect(gear.watercraft).toEqual([]);
    });

    it('should have all expected properties', () => {
      const gear = new Gear();
      
      expect(gear).toHaveProperty('aircraft');
      expect(gear).toHaveProperty('ammunition');
      expect(gear).toHaveProperty('armor');
      expect(gear).toHaveProperty('groundVehicles');
      expect(gear).toHaveProperty('handWeapons');
      expect(gear).toHaveProperty('mundaneItems');
      expect(gear).toHaveProperty('rangedWeapons');
      expect(gear).toHaveProperty('skills');
      expect(gear).toHaveProperty('specialWeapons');
      expect(gear).toHaveProperty('trappingsAndEffects');
      expect(gear).toHaveProperty('vehicleMountedAndAtGuns');
      expect(gear).toHaveProperty('watercraft');
    });

    it('should allow adding items to each category', () => {
      const gear = new Gear();
      
      gear.aircraft.push({ name: 'Fighter Jet', speed: 'Fast' });
      gear.ammunition.push({ type: '9mm', quantity: 50 });
      gear.armor.push({ name: 'Kevlar Vest', protection: 4 });
      gear.groundVehicles.push({ name: 'Jeep', capacity: 4 });
      gear.handWeapons.push({ name: 'Sword', damage: 'Str+d8' });
      gear.mundaneItems.push({ name: 'Rope', length: '50ft' });
      gear.rangedWeapons.push({ name: 'Pistol', range: '12/24/48' });
      gear.skills.push({ name: 'Fighting', die: 'd8' });
      gear.specialWeapons.push({ name: 'Grenade', blast: 'Medium' });
      gear.trappingsAndEffects.push({ name: 'Cloak', effect: 'Stealth' });
      gear.vehicleMountedAndAtGuns.push({ name: 'Cannon', caliber: '20mm' });
      gear.watercraft.push({ name: 'Speedboat', speed: 30 });
      
      expect(gear.aircraft).toHaveLength(1);
      expect(gear.ammunition).toHaveLength(1);
      expect(gear.armor).toHaveLength(1);
      expect(gear.groundVehicles).toHaveLength(1);
      expect(gear.handWeapons).toHaveLength(1);
      expect(gear.mundaneItems).toHaveLength(1);
      expect(gear.rangedWeapons).toHaveLength(1);
      expect(gear.skills).toHaveLength(1);
      expect(gear.specialWeapons).toHaveLength(1);
      expect(gear.trappingsAndEffects).toHaveLength(1);
      expect(gear.vehicleMountedAndAtGuns).toHaveLength(1);
      expect(gear.watercraft).toHaveLength(1);
    });

    it('should create independent instances', () => {
      const gear1 = new Gear();
      const gear2 = new Gear();
      
      gear1.aircraft.push({ name: 'Helicopter' });
      
      expect(gear1.aircraft).toHaveLength(1);
      expect(gear2.aircraft).toHaveLength(0);
    });
  });

  describe('Property Manipulation', () => {
    it('should allow modifying arrays independently', () => {
      const gear = new Gear();
      
      gear.armor = [{ name: 'Plate Mail' }, { name: 'Shield' }];
      gear.handWeapons = [{ name: 'Axe' }];
      
      expect(gear.armor).toHaveLength(2);
      expect(gear.handWeapons).toHaveLength(1);
      
      gear.armor.pop();
      
      expect(gear.armor).toHaveLength(1);
      expect(gear.handWeapons).toHaveLength(1);
    });

    it('should handle empty gear collections', () => {
      const gear = new Gear();
      
      expect(gear.aircraft.length).toBe(0);
      expect(gear.aircraft.pop()).toBeUndefined();
      expect(gear.aircraft.shift()).toBeUndefined();
    });

    it('should support array methods on all properties', () => {
      const gear = new Gear();
      
      // Test array methods
      gear.rangedWeapons.push({ name: 'Bow' }, { name: 'Crossbow' });
      const filtered = gear.rangedWeapons.filter(w => w.name === 'Bow');
      const mapped = gear.rangedWeapons.map(w => w.name);
      
      expect(filtered).toHaveLength(1);
      expect(filtered[0].name).toBe('Bow');
      expect(mapped).toEqual(['Bow', 'Crossbow']);
    });
  });

  describe('Gear Collection Management', () => {
    it('should allow bulk assignment of gear categories', () => {
      const gear = new Gear();
      
      const armorSet = [
        { name: 'Leather', protection: 1 },
        { name: 'Chain', protection: 2 },
        { name: 'Plate', protection: 3 }
      ];
      
      gear.armor = armorSet;
      
      expect(gear.armor).toEqual(armorSet);
      expect(gear.armor).toHaveLength(3);
    });

    it('should maintain reference to assigned arrays', () => {
      const gear = new Gear();
      const weapons = [{ name: 'Spear' }];
      
      gear.handWeapons = weapons;
      weapons.push({ name: 'Mace' });
      
      expect(gear.handWeapons).toHaveLength(2);
      expect(gear.handWeapons).toBe(weapons);
    });

    it('should allow clearing individual categories', () => {
      const gear = new Gear();
      
      gear.specialWeapons = [{ name: 'Flamethrower' }];
      gear.specialWeapons = [];
      
      expect(gear.specialWeapons).toEqual([]);
      expect(gear.specialWeapons).toHaveLength(0);
    });
  });

  describe('Type Checking', () => {
    it('should be an instance of Gear class', () => {
      const gear = new Gear();
      expect(gear).toBeInstanceOf(Gear);
    });

    it('should have array type for all properties', () => {
      const gear = new Gear();
      
      expect(Array.isArray(gear.aircraft)).toBe(true);
      expect(Array.isArray(gear.ammunition)).toBe(true);
      expect(Array.isArray(gear.armor)).toBe(true);
      expect(Array.isArray(gear.groundVehicles)).toBe(true);
      expect(Array.isArray(gear.handWeapons)).toBe(true);
      expect(Array.isArray(gear.mundaneItems)).toBe(true);
      expect(Array.isArray(gear.rangedWeapons)).toBe(true);
      expect(Array.isArray(gear.skills)).toBe(true);
      expect(Array.isArray(gear.specialWeapons)).toBe(true);
      expect(Array.isArray(gear.trappingsAndEffects)).toBe(true);
      expect(Array.isArray(gear.vehicleMountedAndAtGuns)).toBe(true);
      expect(Array.isArray(gear.watercraft)).toBe(true);
    });
  });
});