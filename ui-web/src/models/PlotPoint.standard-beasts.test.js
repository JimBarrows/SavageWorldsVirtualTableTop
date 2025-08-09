import PlotPoint from './PlotPoint'
import Beast from './Beast'

describe('PlotPoint with Standard Beasts', () => {
  describe('constructor', () => {
    test('should initialize with standard beasts', () => {
      const plotPoint = new PlotPoint()
      
      expect(plotPoint.beasts).toBeDefined()
      expect(Array.isArray(plotPoint.beasts)).toBe(true)
      expect(plotPoint.beasts.length).toBeGreaterThan(0)
    })

    test('should have standard beasts as Beast instances', () => {
      const plotPoint = new PlotPoint()
      
      plotPoint.beasts.forEach(beast => {
        expect(beast).toBeInstanceOf(Beast)
      })
    })

    test('should include expected standard beasts', () => {
      const plotPoint = new PlotPoint()
      const beastNames = plotPoint.beasts.map(b => b.name)
      
      // Check for core bestiary creatures
      expect(beastNames).toContain('Alligator/Crocodile')
      expect(beastNames).toContain('Bear')
      expect(beastNames).toContain('Dog/Wolf')
      expect(beastNames).toContain('Ghost')
      expect(beastNames).toContain('Goblin')
      expect(beastNames).toContain('Orc')
      expect(beastNames).toContain('Skeleton')
      expect(beastNames).toContain('Zombie')
    })

    test('should create independent beast instances for each plot point', () => {
      const plotPoint1 = new PlotPoint()
      const plotPoint2 = new PlotPoint()
      
      // Arrays should be different instances
      expect(plotPoint1.beasts).not.toBe(plotPoint2.beasts)
      
      // Individual beasts should be different instances
      expect(plotPoint1.beasts[0]).not.toBe(plotPoint2.beasts[0])
      
      // But should have same initial values
      expect(plotPoint1.beasts[0].name).toBe(plotPoint2.beasts[0].name)
    })
  })

  describe('beast management', () => {
    let plotPoint

    beforeEach(() => {
      plotPoint = new PlotPoint()
    })

    test('should allow adding custom beasts', () => {
      const customBeast = new Beast()
      customBeast.name = 'Custom Dragon'
      customBeast.description = 'A unique dragon variant'
      
      const initialCount = plotPoint.beasts.length
      plotPoint.beasts.push(customBeast)
      
      expect(plotPoint.beasts.length).toBe(initialCount + 1)
      expect(plotPoint.beasts).toContain(customBeast)
    })

    test('should allow removing beasts', () => {
      const initialCount = plotPoint.beasts.length
      const beastToRemove = plotPoint.beasts[0]
      
      plotPoint.beasts = plotPoint.beasts.filter(b => b !== beastToRemove)
      
      expect(plotPoint.beasts.length).toBe(initialCount - 1)
      expect(plotPoint.beasts).not.toContain(beastToRemove)
    })

    test('should allow modifying beast attributes', () => {
      const wolf = plotPoint.beasts.find(b => b.name === 'Dog/Wolf')
      
      expect(wolf).toBeDefined()
      
      // Modify the wolf's strength
      wolf.strength.die = 10
      wolf.description = 'A particularly strong wolf'
      
      // Verify changes persist
      const modifiedWolf = plotPoint.beasts.find(b => b.name === 'Dog/Wolf')
      expect(modifiedWolf.strength.die).toBe(10)
      expect(modifiedWolf.description).toBe('A particularly strong wolf')
    })

    test('should maintain other plot point properties when initialized with beasts', () => {
      const plotPoint = new PlotPoint()
      
      // Check that other default properties are still set correctly
      expect(plotPoint.name).toBe('')
      expect(plotPoint.description).toBe('This is a description')
      expect(plotPoint.powers).toBeDefined()
      expect(plotPoint.skills).toBeDefined()
      expect(plotPoint.characters).toEqual([])
      expect(plotPoint.edges).toEqual([])
      expect(plotPoint.hindrances).toEqual([])
    })
  })

  describe('backward compatibility', () => {
    test('existing code that sets beasts array should still work', () => {
      const plotPoint = new PlotPoint()
      
      // Simulate existing code that might set beasts
      const customBeasts = [
        new Beast(),
        new Beast()
      ]
      customBeasts[0].name = 'Custom Beast 1'
      customBeasts[1].name = 'Custom Beast 2'
      
      // This should replace the standard beasts
      plotPoint.beasts = customBeasts
      
      expect(plotPoint.beasts).toBe(customBeasts)
      expect(plotPoint.beasts.length).toBe(2)
      expect(plotPoint.beasts[0].name).toBe('Custom Beast 1')
    })

    test('existing code that expects empty beasts array can clear it', () => {
      const plotPoint = new PlotPoint()
      
      // Some existing code might want to start with no beasts
      plotPoint.beasts = []
      
      expect(plotPoint.beasts).toEqual([])
      expect(plotPoint.beasts.length).toBe(0)
    })
  })
})