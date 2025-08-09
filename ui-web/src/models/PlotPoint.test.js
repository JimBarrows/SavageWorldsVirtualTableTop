import PlotPoint from './PlotPoint';

describe('PlotPoint', () => {
  describe('constructor', () => {
    it('should initialize with default values', () => {
      const plotPoint = new PlotPoint();
      
      expect(plotPoint.name).toBe('');
      expect(plotPoint.description).toBe('This is a description');
      expect(plotPoint.arcaneBackgrounds).toEqual([]);
      expect(plotPoint.beasts).toBeDefined();
      expect(plotPoint.beasts.length).toBeGreaterThan(0); // Now includes standard beasts
      expect(plotPoint.characters).toEqual([]);
      expect(plotPoint.edges).toEqual([]);
      expect(plotPoint.hindrances).toEqual([]);
      expect(plotPoint.races).toEqual([]);
      expect(plotPoint.settingRules).toEqual([]);
    });

    it('should initialize with default powers', () => {
      const plotPoint = new PlotPoint();
      
      expect(plotPoint.powers).toBeDefined();
      expect(Array.isArray(plotPoint.powers)).toBe(true);
      expect(plotPoint.powers.length).toBeGreaterThan(0);
    });

    it('should initialize with default skills', () => {
      const plotPoint = new PlotPoint();
      
      expect(plotPoint.skills).toBeDefined();
      expect(Array.isArray(plotPoint.skills)).toBe(true);
      expect(plotPoint.skills.length).toBeGreaterThan(0);
      
      // Verify it contains the expected default skills
      const skillNames = plotPoint.skills.map(skill => skill.name);
      expect(skillNames).toContain('Athletics');
      expect(skillNames).toContain('Common Knowledge');
      expect(skillNames).toContain('Notice');
      expect(skillNames).toContain('Persuasion');
      expect(skillNames).toContain('Stealth');
    });

    it('should have exactly 5 default skills', () => {
      const plotPoint = new PlotPoint();
      expect(plotPoint.skills).toHaveLength(5);
    });

    it('should have correct attributes for default skills', () => {
      const plotPoint = new PlotPoint();
      
      const athletics = plotPoint.skills.find(s => s.name === 'Athletics');
      const commonKnowledge = plotPoint.skills.find(s => s.name === 'Common Knowledge');
      const notice = plotPoint.skills.find(s => s.name === 'Notice');
      const persuasion = plotPoint.skills.find(s => s.name === 'Persuasion');
      const stealth = plotPoint.skills.find(s => s.name === 'Stealth');
      
      expect(athletics.attribute).toBe('Agility');
      expect(commonKnowledge.attribute).toBe('Smarts');
      expect(notice.attribute).toBe('Smarts');
      expect(persuasion.attribute).toBe('Spirit');
      expect(stealth.attribute).toBe('Agility');
    });
  });

  describe('skills management', () => {
    it('should allow adding additional skills to the default ones', () => {
      const plotPoint = new PlotPoint();
      const initialCount = plotPoint.skills.length;
      
      plotPoint.skills.push({
        name: 'Fighting',
        attribute: 'Agility',
        description: 'Hand-to-hand combat'
      });
      
      expect(plotPoint.skills.length).toBe(initialCount + 1);
      expect(plotPoint.skills[plotPoint.skills.length - 1].name).toBe('Fighting');
    });

    it('should allow removing skills', () => {
      const plotPoint = new PlotPoint();
      const initialCount = plotPoint.skills.length;
      
      plotPoint.skills.splice(0, 1); // Remove first skill
      
      expect(plotPoint.skills.length).toBe(initialCount - 1);
    });

    it('should allow modifying existing skills', () => {
      const plotPoint = new PlotPoint();
      
      const athletics = plotPoint.skills.find(s => s.name === 'Athletics');
      athletics.attribute = 'Strength';
      
      const modifiedSkill = plotPoint.skills.find(s => s.name === 'Athletics');
      expect(modifiedSkill.attribute).toBe('Strength');
    });
  });
});