export default class CharacterAdvancement {
  constructor() {
    this.experiencePoints = 0;
    this.spentAdvances = 0;
    this.advanceHistory = [];
  }

  addExperience(points) {
    if (points < 0) {
      throw new Error('Experience points cannot be negative');
    }
    this.experiencePoints += points;
  }

  getAvailableAdvances() {
    const totalAdvances = Math.floor(this.experiencePoints / 5);
    return totalAdvances - this.spentAdvances;
  }

  getRank() {
    if (this.experiencePoints >= 80) return 'Legendary';
    if (this.experiencePoints >= 60) return 'Heroic';
    if (this.experiencePoints >= 40) return 'Veteran';
    if (this.experiencePoints >= 20) return 'Seasoned';
    return 'Novice';
  }

  spendAdvance(details = null) {
    if (this.getAvailableAdvances() <= 0) {
      throw new Error('No advances available');
    }
    
    this.spentAdvances++;
    
    if (details) {
      this.advanceHistory.push({
        ...details,
        number: this.spentAdvances,
        timestamp: details.timestamp || new Date()
      });
    }
  }

  applyAttributeAdvance(attribute, from, to) {
    try {
      this.spendAdvance({
        type: 'attribute',
        description: `Increased ${attribute} from ${from} to ${to}`,
        attribute,
        previousValue: from,
        newValue: to
      });
      
      return {
        success: true,
        description: `Increased ${attribute} from ${from} to ${to}`
      };
    } catch (error) {
      return {
        success: false,
        error: error.message
      };
    }
  }

  applyEdgeAdvance(edgeName) {
    try {
      this.spendAdvance({
        type: 'edge',
        description: `Added Edge: ${edgeName}`,
        edge: edgeName
      });
      
      return {
        success: true,
        description: `Added Edge: ${edgeName}`
      };
    } catch (error) {
      return {
        success: false,
        error: error.message
      };
    }
  }

  applySkillAdvance(skills) {
    try {
      const skillDescriptions = skills.map(s => `${s.name} to ${s.to}`).join(', ');
      
      this.spendAdvance({
        type: 'skills',
        description: `Increased ${skillDescriptions}`,
        skills
      });
      
      return {
        success: true,
        description: `Increased ${skillDescriptions}`
      };
    } catch (error) {
      return {
        success: false,
        error: error.message
      };
    }
  }

  applyNewSkillAdvance(skillName, die) {
    try {
      this.spendAdvance({
        type: 'new_skill',
        description: `Added new skill: ${skillName} at ${die}`,
        skill: skillName,
        die
      });
      
      return {
        success: true,
        description: `Added new skill: ${skillName} at ${die}`
      };
    } catch (error) {
      return {
        success: false,
        error: error.message
      };
    }
  }

  canTakeEdge(edgeName, requiredRank) {
    const currentRank = this.getRank();
    const rankOrder = ['Novice', 'Seasoned', 'Veteran', 'Heroic', 'Legendary'];
    
    const currentRankIndex = rankOrder.indexOf(currentRank);
    const requiredRankIndex = rankOrder.indexOf(requiredRank);
    
    return currentRankIndex >= requiredRankIndex;
  }

  canIncreaseAttribute(attribute, from, to) {
    // Check valid dice progression first
    if (!this.isValidDiceProgression(from, to)) {
      return false;
    }
    
    // Additional rank-based restrictions
    const currentRank = this.getRank();
    
    // At Novice rank, can't increase attributes beyond d6
    if (currentRank === 'Novice' && from === 'd6') {
      return false;
    }
    
    return true;
  }

  undoLastAdvance() {
    if (this.advanceHistory.length === 0) {
      return {
        success: false,
        error: 'No advances to undo'
      };
    }
    
    const lastAdvance = this.advanceHistory.pop();
    this.spentAdvances--;
    
    return {
      success: true,
      undone: lastAdvance
    };
  }

  getRankRequirements() {
    return {
      Novice: {
        minXP: 0,
        maxXP: 19,
        minAdvances: 0,
        maxAdvances: 3
      },
      Seasoned: {
        minXP: 20,
        maxXP: 39,
        minAdvances: 4,
        maxAdvances: 7
      },
      Veteran: {
        minXP: 40,
        maxXP: 59,
        minAdvances: 8,
        maxAdvances: 11
      },
      Heroic: {
        minXP: 60,
        maxXP: 79,
        minAdvances: 12,
        maxAdvances: 15
      },
      Legendary: {
        minXP: 80,
        maxXP: null,
        minAdvances: 16,
        maxAdvances: null
      }
    };
  }

  isValidDiceProgression(from, to) {
    const validProgressions = {
      'd4': 'd6',
      'd6': 'd8',
      'd8': 'd10',
      'd10': 'd12',
      'd12': 'd12+1',
      'd12+1': 'd12+2'
    };
    
    return validProgressions[from] === to;
  }

  meetsRequirements(requirements, character) {
    // Check skill requirements
    if (requirements.skills) {
      for (const [skillName, requiredDie] of Object.entries(requirements.skills)) {
        const characterSkill = character[skillName] || character.skills?.[skillName];
        if (!characterSkill || !this.compareDice(characterSkill, requiredDie)) {
          return false;
        }
      }
    }
    
    // Check attribute requirements
    if (requirements.attributes) {
      for (const [attrName, requiredDie] of Object.entries(requirements.attributes)) {
        const characterAttr = character[attrName] || character.attributes?.[attrName];
        if (!characterAttr || !this.compareDice(characterAttr, requiredDie)) {
          return false;
        }
      }
    }
    
    return true;
  }

  compareDice(current, required) {
    const diceOrder = ['d4', 'd6', 'd8', 'd10', 'd12', 'd12+1', 'd12+2'];
    const currentIndex = diceOrder.indexOf(current);
    const requiredIndex = diceOrder.indexOf(required);
    
    return currentIndex >= requiredIndex;
  }

  getProgressToNextRank() {
    const currentRank = this.getRank();
    const requirements = this.getRankRequirements();
    
    if (currentRank === 'Legendary') {
      return { percentage: 100, nextRankXP: null };
    }
    
    const rankOrder = ['Novice', 'Seasoned', 'Veteran', 'Heroic', 'Legendary'];
    const nextRankIndex = rankOrder.indexOf(currentRank) + 1;
    const nextRank = rankOrder[nextRankIndex];
    const nextRankReq = requirements[nextRank];
    
    const currentRankReq = requirements[currentRank];
    const xpInCurrentRank = this.experiencePoints - currentRankReq.minXP;
    const xpNeededForNext = nextRankReq.minXP - currentRankReq.minXP;
    const percentage = (xpInCurrentRank / xpNeededForNext) * 100;
    
    return {
      percentage: Math.min(percentage, 100),
      nextRankXP: nextRankReq.minXP
    };
  }
}