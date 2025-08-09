import Beasts from './Beasts'
import Beast from './Beast'

describe('Beasts', () => {
  describe('getStandardBeasts', () => {
    test('should return an array of standard beasts', () => {
      const standardBeasts = Beasts.getStandardBeasts()
      
      expect(Array.isArray(standardBeasts)).toBe(true)
      expect(standardBeasts.length).toBeGreaterThan(0)
    })

    test('should include essential standard beasts from bestiary', () => {
      const standardBeasts = Beasts.getStandardBeasts()
      const beastNames = standardBeasts.map(beast => beast.name)
      
      // Check for some essential creatures that should be in any bestiary
      expect(beastNames).toContain('Alligator/Crocodile')
      expect(beastNames).toContain('Bear')
      expect(beastNames).toContain('Dog/Wolf')
      expect(beastNames).toContain('Ghost')
      expect(beastNames).toContain('Giant Spider')
      expect(beastNames).toContain('Goblin')
      expect(beastNames).toContain('Horse, Riding')
      expect(beastNames).toContain('Lion')
      expect(beastNames).toContain('Orc')
      expect(beastNames).toContain('Skeleton')
      expect(beastNames).toContain('Swarm')
      expect(beastNames).toContain('Zombie')
    })

    test('each standard beast should be a Beast instance', () => {
      const standardBeasts = Beasts.getStandardBeasts()
      
      standardBeasts.forEach(beast => {
        expect(beast).toBeInstanceOf(Beast)
      })
    })

    test('each standard beast should have required properties', () => {
      const standardBeasts = Beasts.getStandardBeasts()
      
      standardBeasts.forEach(beast => {
        expect(beast).toHaveProperty('name')
        expect(beast).toHaveProperty('description')
        expect(beast).toHaveProperty('agility')
        expect(beast).toHaveProperty('smarts')
        expect(beast).toHaveProperty('spirit')
        expect(beast).toHaveProperty('strength')
        expect(beast).toHaveProperty('vigor')
        expect(beast).toHaveProperty('pace')
        expect(beast).toHaveProperty('armor')
        expect(beast).toHaveProperty('skills')
        expect(beast).toHaveProperty('specialAbilities')
      })
    })

    test('wolf should have correct attributes', () => {
      const standardBeasts = Beasts.getStandardBeasts()
      const wolf = standardBeasts.find(b => b.name === 'Dog/Wolf')
      
      expect(wolf).toBeDefined()
      expect(wolf.name).toBe('Dog/Wolf')
      expect(wolf.agility.die).toBe(8)
      expect(wolf.smarts.die).toBe(4)
      expect(wolf.smarts.modifier).toBe(2) // Animal intelligence
      expect(wolf.spirit.die).toBe(6)
      expect(wolf.strength.die).toBe(6)
      expect(wolf.vigor.die).toBe(6)
      expect(wolf.pace).toBe(8)
      expect(wolf.skills).toEqual(expect.arrayContaining([
        expect.objectContaining({ name: 'Fighting', die: 6 }),
        expect.objectContaining({ name: 'Notice', die: 10 })
      ]))
      expect(wolf.specialAbilities).toEqual(expect.arrayContaining([
        expect.objectContaining({ name: 'Bite', description: 'Str+d4' }),
        expect.objectContaining({ name: 'Fleet-Footed', description: 'Roll a d10 when running instead of a d6' }),
        expect.objectContaining({ name: 'Go for the Throat', description: 'Wolves instinctively go for an opponent\'s soft spots. With a raise on its attack roll, it hits the target\'s most weakly-armored location.' })
      ]))
    })

    test('zombie should have correct attributes', () => {
      const standardBeasts = Beasts.getStandardBeasts()
      const zombie = standardBeasts.find(b => b.name === 'Zombie')
      
      expect(zombie).toBeDefined()
      expect(zombie.name).toBe('Zombie')
      expect(zombie.agility.die).toBe(6)
      expect(zombie.smarts.die).toBe(4)
      expect(zombie.spirit.die).toBe(4)
      expect(zombie.strength.die).toBe(6)
      expect(zombie.vigor.die).toBe(6)
      expect(zombie.pace).toBe(4)
      expect(zombie.armor).toBe(1)
      expect(zombie.skills).toEqual(expect.arrayContaining([
        expect.objectContaining({ name: 'Fighting', die: 6 }),
        expect.objectContaining({ name: 'Intimidation', die: 6 }),
        expect.objectContaining({ name: 'Notice', die: 4 }),
        expect.objectContaining({ name: 'Shooting', die: 6 })
      ]))
      expect(zombie.specialAbilities).toEqual(expect.arrayContaining([
        expect.objectContaining({ name: 'Claws', description: 'Str' }),
        expect.objectContaining({ name: 'Fearless', description: 'Zombies are immune to Fear and Intimidation.' }),
        expect.objectContaining({ name: 'Undead', description: '+2 Toughness; +2 to recover from being Shaken; no additional damage from called shots; immune to disease and poison.' }),
        expect.objectContaining({ name: 'Weakness (Head)', description: 'Shots to a zombie\'s head are +2 damage.' })
      ]))
    })

    test('orc should have correct attributes', () => {
      const standardBeasts = Beasts.getStandardBeasts()
      const orc = standardBeasts.find(b => b.name === 'Orc')
      
      expect(orc).toBeDefined()
      expect(orc.name).toBe('Orc')
      expect(orc.agility.die).toBe(6)
      expect(orc.smarts.die).toBe(4)
      expect(orc.spirit.die).toBe(6)
      expect(orc.strength.die).toBe(8)
      expect(orc.vigor.die).toBe(8)
      expect(orc.pace).toBe(6)
      expect(orc.armor).toBe(1) // Thick hide
      expect(orc.skills).toEqual(expect.arrayContaining([
        expect.objectContaining({ name: 'Fighting', die: 6 }),
        expect.objectContaining({ name: 'Intimidation', die: 8 }),
        expect.objectContaining({ name: 'Notice', die: 6 }),
        expect.objectContaining({ name: 'Shooting', die: 6 }),
        expect.objectContaining({ name: 'Stealth', die: 4 }),
        expect.objectContaining({ name: 'Throwing', die: 6 })
      ]))
    })

    test('standard beasts should be new instances each time', () => {
      const beasts1 = Beasts.getStandardBeasts()
      const beasts2 = Beasts.getStandardBeasts()
      
      // Should be different array instances
      expect(beasts1).not.toBe(beasts2)
      
      // Should be different beast instances
      expect(beasts1[0]).not.toBe(beasts2[0])
      
      // But should have same values
      expect(beasts1[0].name).toBe(beasts2[0].name)
    })
  })

  describe('createBeast', () => {
    test('should create a beast with provided attributes', () => {
      const beastData = {
        name: 'Dragon',
        description: 'A mighty winged lizard',
        agility: { die: 8, modifier: 0 },
        smarts: { die: 8, modifier: 0 },
        spirit: { die: 10, modifier: 0 },
        strength: { die: 12, modifier: 6 },
        vigor: { die: 12, modifier: 0 },
        pace: 8,
        armor: 4,
        skills: [
          { name: 'Fighting', die: 10 },
          { name: 'Intimidation', die: 12 },
          { name: 'Notice', die: 8 }
        ],
        specialAbilities: [
          { name: 'Armor +4', description: 'Scaly hide.' },
          { name: 'Bite/Claws', description: 'Str+d8' },
          { name: 'Fear -2', description: 'Anyone who sees a mighty dragon must make a Fear check at -2.' },
          { name: 'Fiery Breath', description: 'Dragons breathe fire using the Cone Template. Every target within this cone may make an Agility roll at -2 to avoid the attack. Damage is 2d10, and victims have a chance of catching fire.' },
          { name: 'Flight', description: 'Dragons have a Flying Pace of 24".' },
          { name: 'Hardy', description: 'The creature does not suffer a wound from being Shaken twice.' },
          { name: 'Huge', description: 'Attackers add +4 to their Fighting or Shooting rolls when attacking a dragon due to its massive size.' },
          { name: 'Improved Arcane Resistance', description: 'Dragons have +4 to resist magical effects.' },
          { name: 'Tail Lash', description: 'The dragon can sweep all opponents in its rear facing in a 3" long by 6" wide square. This is a standard Fighting attack, and damage is Str-2.' }
        ]
      }
      
      const beast = Beasts.createBeast(beastData)
      
      expect(beast).toBeInstanceOf(Beast)
      expect(beast.name).toBe('Dragon')
      expect(beast.description).toBe('A mighty winged lizard')
      expect(beast.agility.die).toBe(8)
      expect(beast.strength.die).toBe(12)
      expect(beast.strength.modifier).toBe(6)
      expect(beast.pace).toBe(8)
      expect(beast.armor).toBe(4)
      expect(beast.skills).toHaveLength(3)
      expect(beast.specialAbilities).toHaveLength(9)
    })

    test('should use default values when not provided', () => {
      const beastData = {
        name: 'Simple Creature'
      }
      
      const beast = Beasts.createBeast(beastData)
      
      expect(beast.name).toBe('Simple Creature')
      expect(beast.description).toBe('')
      expect(beast.pace).toBe(6)
      expect(beast.armor).toBe(0)
      expect(beast.skills).toEqual([])
      expect(beast.specialAbilities).toEqual([])
    })
  })
})