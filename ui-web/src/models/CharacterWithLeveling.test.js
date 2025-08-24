const Character = require('./Character').default;
const CharacterAdvancement = require('./CharacterAdvancement').default;

describe('Character with Leveling System', () => {
  let character;

  beforeEach(() => {
    character = new Character();
    character.name = 'Test Hero';
    character.advancement = new CharacterAdvancement();
  });

  describe('Character Experience Integration', () => {
    it('should initialize with advancement system', () => {
      expect(character.advancement).toBeDefined();
      expect(character.advancement.experiencePoints).toBe(0);
    });

    it('should track experience points on character', () => {
      character.addExperience(10);
      expect(character.advancement.experiencePoints).toBe(10);
      expect(character.getExperience()).toBe(10);
    });

    it('should calculate rank correctly', () => {
      expect(character.getRank()).toBe('Novice');
      
      character.addExperience(20);
      expect(character.getRank()).toBe('Seasoned');
    });

    it('should track available advances', () => {
      character.addExperience(5);
      expect(character.getAvailableAdvances()).toBe(1);
    });
  });

  describe('Attribute Advancement', () => {
    beforeEach(() => {
      character.addExperience(5); // 1 advance available
    });

    it('should increase attribute with advance', () => {
      expect(character.agility.die).toBe('d4');
      
      const result = character.increaseAttribute('agility');
      expect(result.success).toBe(true);
      expect(character.agility.die).toBe('d6');
      expect(character.getAvailableAdvances()).toBe(0);
    });

    it('should record attribute increase in history', () => {
      character.increaseAttribute('vigor');
      const history = character.getAdvanceHistory();
      
      expect(history).toHaveLength(1);
      expect(history[0].type).toBe('attribute');
      expect(history[0].description).toContain('vigor');
    });

    it('should prevent invalid attribute increases', () => {
      character.strength.die = 'd12';
      const result = character.increaseAttribute('strength');
      
      expect(result.success).toBe(false);
      expect(result.error).toContain('cannot increase');
    });
  });

  describe('Edge Management', () => {
    beforeEach(() => {
      character.addExperience(10); // 2 advances available
    });

    it('should add edge with advance', () => {
      const result = character.addEdgeWithAdvance('Quick');
      
      expect(result.success).toBe(true);
      expect(character.edges).toContain('Quick');
      expect(character.getAvailableAdvances()).toBe(1);
    });

    it('should check edge requirements', () => {
      const requirements = {
        rank: 'Seasoned',
        attributes: { Agility: 'd8' }
      };
      
      const canTake = character.canTakeEdge('Fleet-Footed', requirements);
      expect(canTake).toBe(false); // Not Seasoned yet
    });

    it('should prevent duplicate edges', () => {
      character.addEdgeWithAdvance('Brawny');
      const result = character.addEdgeWithAdvance('Brawny');
      
      expect(result.success).toBe(false);
      expect(result.error).toContain('already has');
    });
  });

  describe('Skill Advancement', () => {
    beforeEach(() => {
      character.addExperience(10); // 2 advances available
      character.skills = [
        { name: 'Fighting', die: 'd4' },
        { name: 'Notice', die: 'd4' },
        { name: 'Shooting', die: 'd6' }
      ];
    });

    it('should increase two skills with one advance', () => {
      const skills = ['Fighting', 'Notice'];
      const result = character.increaseSkills(skills);
      
      expect(result.success).toBe(true);
      expect(character.getSkill('Fighting').die).toBe('d6');
      expect(character.getSkill('Notice').die).toBe('d6');
      expect(character.getAvailableAdvances()).toBe(1);
    });

    it('should add new skill with advance', () => {
      const result = character.addNewSkill('Stealth', 'd4');
      
      expect(result.success).toBe(true);
      expect(character.getSkill('Stealth')).toBeDefined();
      expect(character.getSkill('Stealth').die).toBe('d4');
    });

    it('should prevent increasing skill beyond attribute', () => {
      character.agility.die = 'd6';
      character.skills = [{ name: 'Fighting', die: 'd6', linkedAttribute: 'agility' }];
      
      const result = character.increaseSkills(['Fighting']);
      expect(result.success).toBe(false);
      expect(result.error).toContain('cannot exceed');
    });
  });

  describe('Complete Advancement Workflow', () => {
    it('should handle full progression from Novice to Seasoned', () => {
      // Start as Novice
      expect(character.getRank()).toBe('Novice');
      
      // First advance at 5 XP - increase attribute
      character.addExperience(5);
      character.increaseAttribute('vigor');
      expect(character.vigor.die).toBe('d6');
      
      // Second advance at 10 XP - add edge
      character.addExperience(5);
      character.addEdgeWithAdvance('Brawny');
      expect(character.edges).toContain('Brawny');
      
      // Third advance at 15 XP - increase skills
      character.addExperience(5);
      character.skills = [
        { name: 'Fighting', die: 'd4' },
        { name: 'Notice', die: 'd4' }
      ];
      character.increaseSkills(['Fighting', 'Notice']);
      
      // Fourth advance at 20 XP - become Seasoned
      character.addExperience(5);
      expect(character.getRank()).toBe('Seasoned');
      expect(character.getAvailableAdvances()).toBe(1);
    });
  });

  describe('Advance History and Undo', () => {
    beforeEach(() => {
      character.addExperience(15); // 3 advances
      character.vigor.die = 'd4';
    });

    it('should maintain complete advance history', () => {
      character.increaseAttribute('vigor');
      character.addEdgeWithAdvance('Brawny');
      
      const history = character.getAdvanceHistory();
      expect(history).toHaveLength(2);
      expect(history[0].type).toBe('attribute');
      expect(history[1].type).toBe('edge');
    });

    it('should undo attribute advance', () => {
      character.increaseAttribute('vigor');
      expect(character.vigor.die).toBe('d6');
      
      const result = character.undoLastAdvance();
      expect(result.success).toBe(true);
      expect(character.vigor.die).toBe('d4');
      expect(character.getAvailableAdvances()).toBe(3);
    });

    it('should undo edge advance', () => {
      character.addEdgeWithAdvance('Quick');
      expect(character.edges).toContain('Quick');
      
      const result = character.undoLastAdvance();
      expect(result.success).toBe(true);
      expect(character.edges).not.toContain('Quick');
    });

    it('should undo skill advance', () => {
      character.skills = [{ name: 'Fighting', die: 'd4' }];
      character.increaseSkills(['Fighting']);
      expect(character.getSkill('Fighting').die).toBe('d6');
      
      const result = character.undoLastAdvance();
      expect(result.success).toBe(true);
      expect(character.getSkill('Fighting').die).toBe('d4');
    });
  });

  describe('Validation and Business Rules', () => {
    it('should enforce one attribute per rank limit', () => {
      character.addExperience(15); // 3 advances at Novice
      
      // First attribute increase should work
      character.increaseAttribute('vigor');
      expect(character.vigor.die).toBe('d6');
      
      // Second should fail at Novice rank
      const result = character.increaseAttribute('agility');
      expect(result.success).toBe(false);
      expect(result.error).toContain('rank limit');
    });

    it('should allow more attributes at higher ranks', () => {
      character.addExperience(20); // Seasoned rank
      
      character.increaseAttribute('vigor');
      character.increaseAttribute('agility');
      
      expect(character.vigor.die).toBe('d6');
      expect(character.agility.die).toBe('d6');
    });

    it('should track advances spent vs total earned', () => {
      character.addExperience(20); // 4 advances
      
      expect(character.getTotalAdvances()).toBe(4);
      expect(character.getSpentAdvances()).toBe(0);
      
      character.increaseAttribute('vigor');
      expect(character.getSpentAdvances()).toBe(1);
      expect(character.getAvailableAdvances()).toBe(3);
    });
  });

  describe('Character Sheet Display Data', () => {
    it('should provide formatted advancement data for display', () => {
      character.addExperience(25);
      character.increaseAttribute('vigor');
      
      const displayData = character.getAdvancementDisplay();
      
      expect(displayData).toEqual({
        experiencePoints: 25,
        rank: 'Seasoned',
        totalAdvances: 5,
        spentAdvances: 1,
        availableAdvances: 4,
        nextRankAt: 40,
        progressToNextRank: '62.5%'
      });
    });

    it('should format advance history for display', () => {
      character.addExperience(10);
      character.increaseAttribute('vigor');
      character.addEdgeWithAdvance('Brawny');
      
      const historyDisplay = character.getFormattedAdvanceHistory();
      
      expect(historyDisplay).toHaveLength(2);
      expect(historyDisplay[0]).toHaveProperty('number', 1);
      expect(historyDisplay[0]).toHaveProperty('type', 'attribute');
      expect(historyDisplay[0]).toHaveProperty('description');
      expect(historyDisplay[0]).toHaveProperty('date');
    });
  });

  describe('Helper Methods', () => {
    beforeEach(() => {
      character.skills = [
        { name: 'Fighting', die: 'd6' },
        { name: 'Notice', die: 'd4' }
      ];
    });

    it('should get skill by name', () => {
      const skill = character.getSkill('Fighting');
      expect(skill).toBeDefined();
      expect(skill.die).toBe('d6');
    });

    it('should return undefined for non-existent skill', () => {
      const skill = character.getSkill('Stealth');
      expect(skill).toBeUndefined();
    });

    it('should check if character has edge', () => {
      character.edges = ['Quick', 'Brawny'];
      
      expect(character.hasEdge('Quick')).toBe(true);
      expect(character.hasEdge('Fleet-Footed')).toBe(false);
    });

    it('should calculate derived stats after advancement', () => {
      character.vigor.die = 'd6';
      character.increaseAttribute('vigor'); // d6 to d8
      
      const toughness = character.calculateToughness();
      expect(toughness).toBe(6); // Base 2 + Vigor d8/2 = 6
    });
  });
});