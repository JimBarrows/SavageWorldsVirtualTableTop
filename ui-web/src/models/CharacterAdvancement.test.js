import CharacterAdvancement from './CharacterAdvancement';

describe('CharacterAdvancement', () => {
  let advancement;

  beforeEach(() => {
    advancement = new CharacterAdvancement();
  });

  describe('Experience Points Management', () => {
    it('should initialize with 0 experience points', () => {
      expect(advancement.experiencePoints).toBe(0);
    });

    it('should add experience points correctly', () => {
      advancement.addExperience(5);
      expect(advancement.experiencePoints).toBe(5);
    });

    it('should calculate available advances based on experience', () => {
      advancement.addExperience(5);
      expect(advancement.getAvailableAdvances()).toBe(1);
      
      advancement.addExperience(5); // Total: 10
      expect(advancement.getAvailableAdvances()).toBe(2);
      
      advancement.addExperience(10); // Total: 20
      expect(advancement.getAvailableAdvances()).toBe(4);
    });

    it('should not allow negative experience points', () => {
      expect(() => advancement.addExperience(-5)).toThrow('Experience points cannot be negative');
    });
  });

  describe('Rank Calculation', () => {
    it('should start at Novice rank', () => {
      expect(advancement.getRank()).toBe('Novice');
    });

    it('should advance to Seasoned at 20 experience points', () => {
      advancement.addExperience(20);
      expect(advancement.getRank()).toBe('Seasoned');
    });

    it('should advance to Veteran at 40 experience points', () => {
      advancement.addExperience(40);
      expect(advancement.getRank()).toBe('Veteran');
    });

    it('should advance to Heroic at 60 experience points', () => {
      advancement.addExperience(60);
      expect(advancement.getRank()).toBe('Heroic');
    });

    it('should advance to Legendary at 80 experience points', () => {
      advancement.addExperience(80);
      expect(advancement.getRank()).toBe('Legendary');
    });

    it('should remain Legendary beyond 80 experience points', () => {
      advancement.addExperience(100);
      expect(advancement.getRank()).toBe('Legendary');
    });
  });

  describe('Advance Spending', () => {
    beforeEach(() => {
      advancement.addExperience(5); // 1 advance available
    });

    it('should track spent advances', () => {
      expect(advancement.spentAdvances).toBe(0);
      advancement.spendAdvance();
      expect(advancement.spentAdvances).toBe(1);
    });

    it('should reduce available advances when spent', () => {
      expect(advancement.getAvailableAdvances()).toBe(1);
      advancement.spendAdvance();
      expect(advancement.getAvailableAdvances()).toBe(0);
    });

    it('should not allow spending advances when none available', () => {
      advancement.spendAdvance(); // Spend the one available
      expect(() => advancement.spendAdvance()).toThrow('No advances available');
    });

    it('should track advance history', () => {
      const advanceDetails = {
        type: 'attribute',
        description: 'Increased Agility to d6',
        timestamp: new Date()
      };
      
      advancement.spendAdvance(advanceDetails);
      expect(advancement.advanceHistory).toHaveLength(1);
      expect(advancement.advanceHistory[0]).toMatchObject(advanceDetails);
    });
  });

  describe('Advance Types', () => {
    beforeEach(() => {
      advancement.addExperience(5); // 1 advance available
    });

    it('should allow attribute advancement', () => {
      const result = advancement.applyAttributeAdvance('agility', 'd4', 'd6');
      expect(result.success).toBe(true);
      expect(result.description).toBe('Increased agility from d4 to d6');
    });

    it('should allow edge advancement', () => {
      const result = advancement.applyEdgeAdvance('Quick');
      expect(result.success).toBe(true);
      expect(result.description).toBe('Added Edge: Quick');
    });

    it('should allow skill advancement', () => {
      const skills = [
        { name: 'Fighting', from: 'd4', to: 'd6' },
        { name: 'Notice', from: 'd4', to: 'd6' }
      ];
      const result = advancement.applySkillAdvance(skills);
      expect(result.success).toBe(true);
      expect(result.description).toBe('Increased Fighting to d6, Notice to d6');
    });

    it('should allow new skill advancement', () => {
      const result = advancement.applyNewSkillAdvance('Stealth', 'd4');
      expect(result.success).toBe(true);
      expect(result.description).toBe('Added new skill: Stealth at d4');
    });
  });

  describe('Rank Requirements Validation', () => {
    it('should enforce Novice rank edge requirements', () => {
      const canTake = advancement.canTakeEdge('Quick', 'Novice');
      expect(canTake).toBe(true);
    });

    it('should prevent taking Veteran edges at Novice rank', () => {
      const canTake = advancement.canTakeEdge('Master Fighter', 'Veteran');
      expect(canTake).toBe(false);
    });

    it('should allow Veteran edges at Veteran rank', () => {
      advancement.addExperience(40); // Veteran rank
      const canTake = advancement.canTakeEdge('Master Fighter', 'Veteran');
      expect(canTake).toBe(true);
    });

    it('should validate attribute limits by rank', () => {
      // Novice can only have one attribute at d6
      const canIncrease = advancement.canIncreaseAttribute('agility', 'd6', 'd8');
      expect(canIncrease).toBe(false);
    });
  });

  describe('Advance History Management', () => {
    beforeEach(() => {
      advancement.addExperience(15); // 3 advances available
    });

    it('should maintain chronological advance history', () => {
      advancement.applyAttributeAdvance('vigor', 'd4', 'd6');
      advancement.applyEdgeAdvance('Brawny');
      
      expect(advancement.advanceHistory).toHaveLength(2);
      expect(advancement.advanceHistory[0].description).toContain('vigor');
      expect(advancement.advanceHistory[1].description).toContain('Brawny');
    });

    it('should allow undoing the last advance', () => {
      advancement.applyAttributeAdvance('vigor', 'd4', 'd6');
      const historyLength = advancement.advanceHistory.length;
      
      const result = advancement.undoLastAdvance();
      expect(result.success).toBe(true);
      expect(advancement.advanceHistory).toHaveLength(historyLength - 1);
      expect(advancement.spentAdvances).toBe(0);
    });

    it('should not allow undoing when no history exists', () => {
      const result = advancement.undoLastAdvance();
      expect(result.success).toBe(false);
      expect(result.error).toBe('No advances to undo');
    });

    it('should restore advance point when undoing', () => {
      advancement.applyAttributeAdvance('vigor', 'd4', 'd6');
      const availableBefore = advancement.getAvailableAdvances();
      
      advancement.undoLastAdvance();
      const availableAfter = advancement.getAvailableAdvances();
      
      expect(availableAfter).toBe(availableBefore + 1);
    });
  });

  describe('Rank Progression Table', () => {
    it('should provide correct rank requirements', () => {
      const requirements = advancement.getRankRequirements();
      
      expect(requirements.Novice).toEqual({
        minXP: 0,
        maxXP: 19,
        minAdvances: 0,
        maxAdvances: 3
      });
      
      expect(requirements.Seasoned).toEqual({
        minXP: 20,
        maxXP: 39,
        minAdvances: 4,
        maxAdvances: 7
      });
      
      expect(requirements.Veteran).toEqual({
        minXP: 40,
        maxXP: 59,
        minAdvances: 8,
        maxAdvances: 11
      });
      
      expect(requirements.Heroic).toEqual({
        minXP: 60,
        maxXP: 79,
        minAdvances: 12,
        maxAdvances: 15
      });
      
      expect(requirements.Legendary).toEqual({
        minXP: 80,
        maxXP: null,
        minAdvances: 16,
        maxAdvances: null
      });
    });
  });

  describe('Validation Rules', () => {
    it('should validate dice progression', () => {
      expect(advancement.isValidDiceProgression('d4', 'd6')).toBe(true);
      expect(advancement.isValidDiceProgression('d6', 'd8')).toBe(true);
      expect(advancement.isValidDiceProgression('d4', 'd8')).toBe(false);
      expect(advancement.isValidDiceProgression('d12', 'd12+1')).toBe(true);
    });

    it('should validate skill requirements for edges', () => {
      const requirements = {
        skills: { Fighting: 'd8', Shooting: 'd6' }
      };
      
      const characterSkills = {
        Fighting: 'd8',
        Shooting: 'd6',
        Notice: 'd4'
      };
      
      expect(advancement.meetsRequirements(requirements, characterSkills)).toBe(true);
    });

    it('should validate attribute requirements for edges', () => {
      const requirements = {
        attributes: { Agility: 'd8' }
      };
      
      const characterAttributes = {
        Agility: 'd6',
        Smarts: 'd6'
      };
      
      expect(advancement.meetsRequirements(requirements, characterAttributes)).toBe(false);
    });
  });

  describe('Advance Calculation Edge Cases', () => {
    it('should handle partial advance points correctly', () => {
      advancement.addExperience(4); // Not enough for an advance
      expect(advancement.getAvailableAdvances()).toBe(0);
      
      advancement.addExperience(1); // Now 5 total
      expect(advancement.getAvailableAdvances()).toBe(1);
    });

    it('should calculate advances correctly across rank boundaries', () => {
      advancement.addExperience(19); // 3 advances, still Novice
      expect(advancement.getAvailableAdvances()).toBe(3);
      expect(advancement.getRank()).toBe('Novice');
      
      advancement.addExperience(1); // 20 total, now Seasoned
      expect(advancement.getAvailableAdvances()).toBe(4);
      expect(advancement.getRank()).toBe('Seasoned');
    });
  });
});