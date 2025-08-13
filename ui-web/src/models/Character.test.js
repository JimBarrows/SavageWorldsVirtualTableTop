import Character from './Character';

describe('Character Model', () => {
  describe('Constructor', () => {
    it('creates a character with default values', () => {
      const character = new Character();
      
      expect(character.id).toBeUndefined();
      expect(character.name).toBe('');
      expect(character.race).toBe('Human');
      expect(character.attributes).toBeDefined();
      expect(character.skills).toEqual([]);
      expect(character.edges).toEqual([]);
      expect(character.hindrances).toEqual([]);
      expect(character.gear).toEqual([]);
      expect(character.wounds).toBe(0);
      expect(character.fatigue).toBe(0);
      expect(character.bennies).toBe(3);
    });

    it('creates a character with provided data', () => {
      const data = {
        id: '123',
        name: 'Test Hero',
        race: 'Elf',
        attributes: {
          agility: 'd8',
          smarts: 'd6',
          spirit: 'd6',
          strength: 'd6',
          vigor: 'd8'
        },
        skills: [{ name: 'Fighting', die: 'd8' }],
        edges: ['Quick'],
        hindrances: ['Curious'],
        bennies: 2
      };
      
      const character = new Character(data);
      
      expect(character.id).toBe('123');
      expect(character.name).toBe('Test Hero');
      expect(character.race).toBe('Elf');
      expect(character.attributes.agility).toBe('d8');
      expect(character.skills).toHaveLength(1);
      expect(character.edges).toContain('Quick');
      expect(character.hindrances).toContain('Curious');
      expect(character.bennies).toBe(2);
    });
  });

  describe('Attributes', () => {
    it('initializes default attributes', () => {
      const character = new Character();
      
      expect(character.attributes).toEqual({
        agility: 'd4',
        smarts: 'd4',
        spirit: 'd4',
        strength: 'd4',
        vigor: 'd4'
      });
    });

    it('validates attribute values', () => {
      const character = new Character();
      
      expect(character.isValidAttribute('d4')).toBe(true);
      expect(character.isValidAttribute('d6')).toBe(true);
      expect(character.isValidAttribute('d8')).toBe(true);
      expect(character.isValidAttribute('d10')).toBe(true);
      expect(character.isValidAttribute('d12')).toBe(true);
      expect(character.isValidAttribute('d12+1')).toBe(true);
      expect(character.isValidAttribute('d20')).toBe(false);
      expect(character.isValidAttribute('invalid')).toBe(false);
    });

    it('sets attribute value', () => {
      const character = new Character();
      
      character.setAttribute('agility', 'd8');
      expect(character.attributes.agility).toBe('d8');
    });

    it('throws error for invalid attribute value', () => {
      const character = new Character();
      
      expect(() => {
        character.setAttribute('agility', 'd20');
      }).toThrow('Invalid attribute value');
    });

    it('calculates attribute modifiers', () => {
      const character = new Character();
      
      expect(character.getAttributeModifier('d4')).toBe(0);
      expect(character.getAttributeModifier('d6')).toBe(1);
      expect(character.getAttributeModifier('d8')).toBe(2);
      expect(character.getAttributeModifier('d10')).toBe(3);
      expect(character.getAttributeModifier('d12')).toBe(4);
      expect(character.getAttributeModifier('d12+1')).toBe(5);
    });
  });

  describe('Skills', () => {
    it('adds a skill', () => {
      const character = new Character();
      
      character.addSkill('Fighting', 'd8');
      
      expect(character.skills).toHaveLength(1);
      expect(character.skills[0]).toEqual({
        name: 'Fighting',
        die: 'd8',
        attribute: 'agility'
      });
    });

    it('updates existing skill', () => {
      const character = new Character();
      
      character.addSkill('Fighting', 'd6');
      character.addSkill('Fighting', 'd8');
      
      expect(character.skills).toHaveLength(1);
      expect(character.skills[0].die).toBe('d8');
    });

    it('removes a skill', () => {
      const character = new Character();
      
      character.addSkill('Fighting', 'd8');
      character.addSkill('Shooting', 'd6');
      character.removeSkill('Fighting');
      
      expect(character.skills).toHaveLength(1);
      expect(character.skills[0].name).toBe('Shooting');
    });

    it('gets skill by name', () => {
      const character = new Character();
      
      character.addSkill('Fighting', 'd8');
      const skill = character.getSkill('Fighting');
      
      expect(skill).toBeDefined();
      expect(skill.die).toBe('d8');
    });

    it('returns undefined for non-existent skill', () => {
      const character = new Character();
      
      const skill = character.getSkill('NonExistent');
      expect(skill).toBeUndefined();
    });

    it('validates skill die values', () => {
      const character = new Character();
      
      expect(() => {
        character.addSkill('Fighting', 'd20');
      }).toThrow('Invalid skill die value');
    });
  });

  describe('Edges and Hindrances', () => {
    it('adds edges', () => {
      const character = new Character();
      
      character.addEdge('Quick');
      character.addEdge('Brave');
      
      expect(character.edges).toHaveLength(2);
      expect(character.edges).toContain('Quick');
      expect(character.edges).toContain('Brave');
    });

    it('prevents duplicate edges', () => {
      const character = new Character();
      
      character.addEdge('Quick');
      character.addEdge('Quick');
      
      expect(character.edges).toHaveLength(1);
    });

    it('removes edges', () => {
      const character = new Character();
      
      character.addEdge('Quick');
      character.addEdge('Brave');
      character.removeEdge('Quick');
      
      expect(character.edges).toHaveLength(1);
      expect(character.edges).not.toContain('Quick');
    });

    it('adds hindrances', () => {
      const character = new Character();
      
      character.addHindrance('Curious', 'Minor');
      
      expect(character.hindrances).toHaveLength(1);
      expect(character.hindrances[0]).toEqual({
        name: 'Curious',
        severity: 'Minor'
      });
    });

    it('validates hindrance severity', () => {
      const character = new Character();
      
      expect(() => {
        character.addHindrance('Bad Eyes', 'Invalid');
      }).toThrow('Invalid hindrance severity');
    });

    it('removes hindrances', () => {
      const character = new Character();
      
      character.addHindrance('Curious', 'Minor');
      character.addHindrance('Bad Eyes', 'Major');
      character.removeHindrance('Curious');
      
      expect(character.hindrances).toHaveLength(1);
      expect(character.hindrances[0].name).toBe('Bad Eyes');
    });
  });

  describe('Derived Statistics', () => {
    it('calculates pace', () => {
      const character = new Character();
      
      expect(character.getPace()).toBe(6);
      
      character.addEdge('Fleet-Footed');
      expect(character.getPace()).toBe(8);
    });

    it('calculates parry', () => {
      const character = new Character();
      
      expect(character.getParry()).toBe(2); // Base without Fighting skill
      
      character.addSkill('Fighting', 'd8');
      expect(character.getParry()).toBe(6); // 2 + d8/2
    });

    it('calculates toughness', () => {
      const character = new Character();
      
      character.setAttribute('vigor', 'd6');
      expect(character.getToughness()).toBe(5); // 2 + d6/2
      
      character.setAttribute('vigor', 'd8');
      expect(character.getToughness()).toBe(6); // 2 + d8/2
    });

    it('calculates charisma', () => {
      const character = new Character();
      
      expect(character.getCharisma()).toBe(0);
      
      character.addEdge('Attractive');
      expect(character.getCharisma()).toBe(2);
      
      character.addHindrance('Mean', 'Minor');
      expect(character.getCharisma()).toBe(0);
    });
  });

  describe('Wounds and Status', () => {
    it('applies wounds', () => {
      const character = new Character();
      
      character.applyWound();
      expect(character.wounds).toBe(1);
      
      character.applyWound(2);
      expect(character.wounds).toBe(3);
    });

    it('limits wounds to maximum', () => {
      const character = new Character();
      
      character.applyWound(5);
      expect(character.wounds).toBe(3); // Max is 3 for normal characters
    });

    it('heals wounds', () => {
      const character = new Character();
      
      character.wounds = 3;
      character.healWound();
      expect(character.wounds).toBe(2);
      
      character.healWound(2);
      expect(character.wounds).toBe(0);
    });

    it('checks if character is incapacitated', () => {
      const character = new Character();
      
      expect(character.isIncapacitated()).toBe(false);
      
      character.wounds = 3;
      expect(character.isIncapacitated()).toBe(true);
    });

    it('applies fatigue', () => {
      const character = new Character();
      
      character.applyFatigue();
      expect(character.fatigue).toBe(1);
      
      character.applyFatigue(2);
      expect(character.fatigue).toBe(3);
    });

    it('limits fatigue to maximum', () => {
      const character = new Character();
      
      character.applyFatigue(5);
      expect(character.fatigue).toBe(3); // Max is 3
    });

    it('recovers from fatigue', () => {
      const character = new Character();
      
      character.fatigue = 2;
      character.recoverFatigue();
      expect(character.fatigue).toBe(1);
      
      character.recoverFatigue(1);
      expect(character.fatigue).toBe(0);
    });

    it('checks if character is exhausted', () => {
      const character = new Character();
      
      expect(character.isExhausted()).toBe(false);
      
      character.fatigue = 3;
      expect(character.isExhausted()).toBe(true);
    });
  });

  describe('Bennies', () => {
    it('uses bennies', () => {
      const character = new Character();
      
      expect(character.bennies).toBe(3);
      
      character.useBenny();
      expect(character.bennies).toBe(2);
    });

    it('prevents using bennies when none available', () => {
      const character = new Character();
      character.bennies = 0;
      
      expect(character.useBenny()).toBe(false);
      expect(character.bennies).toBe(0);
    });

    it('awards bennies', () => {
      const character = new Character();
      
      character.awardBenny();
      expect(character.bennies).toBe(4);
      
      character.awardBenny(2);
      expect(character.bennies).toBe(6);
    });

    it('resets bennies to starting value', () => {
      const character = new Character();
      
      character.bennies = 1;
      character.resetBennies();
      expect(character.bennies).toBe(3);
    });
  });

  describe('Experience and Advancement', () => {
    it('tracks experience points', () => {
      const character = new Character();
      
      expect(character.experience).toBe(0);
      
      character.awardExperience(5);
      expect(character.experience).toBe(5);
    });

    it('calculates rank based on experience', () => {
      const character = new Character();
      
      expect(character.getRank()).toBe('Novice');
      
      character.experience = 20;
      expect(character.getRank()).toBe('Seasoned');
      
      character.experience = 40;
      expect(character.getRank()).toBe('Veteran');
      
      character.experience = 60;
      expect(character.getRank()).toBe('Heroic');
      
      character.experience = 80;
      expect(character.getRank()).toBe('Legendary');
    });

    it('tracks advances', () => {
      const character = new Character();
      
      character.awardExperience(5);
      expect(character.getAdvances()).toBe(1);
      
      character.awardExperience(15);
      expect(character.getAdvances()).toBe(4);
    });
  });

  describe('Serialization', () => {
    it('converts to JSON', () => {
      const character = new Character({
        name: 'Test Hero',
        race: 'Human',
        attributes: { agility: 'd8' }
      });
      
      const json = character.toJSON();
      
      expect(json).toHaveProperty('name', 'Test Hero');
      expect(json).toHaveProperty('race', 'Human');
      expect(json).toHaveProperty('attributes');
      expect(json.attributes).toHaveProperty('agility', 'd8');
    });

    it('creates from JSON', () => {
      const json = {
        name: 'Test Hero',
        race: 'Elf',
        attributes: { agility: 'd10' },
        skills: [{ name: 'Fighting', die: 'd8' }],
        edges: ['Quick'],
        wounds: 1,
        bennies: 2
      };
      
      const character = Character.fromJSON(json);
      
      expect(character.name).toBe('Test Hero');
      expect(character.race).toBe('Elf');
      expect(character.attributes.agility).toBe('d10');
      expect(character.skills).toHaveLength(1);
      expect(character.edges).toContain('Quick');
      expect(character.wounds).toBe(1);
      expect(character.bennies).toBe(2);
    });

    it('validates JSON data on creation', () => {
      const invalidJson = {
        name: 'Test',
        attributes: { agility: 'invalid' }
      };
      
      expect(() => {
        Character.fromJSON(invalidJson);
      }).toThrow();
    });
  });

  describe('Validation', () => {
    it('validates character completeness', () => {
      const character = new Character();
      
      expect(character.isValid()).toBe(false); // No name
      
      character.name = 'Test Hero';
      expect(character.isValid()).toBe(true);
    });

    it('validates character for game rules', () => {
      const character = new Character();
      character.name = 'Test';
      
      // Check attribute points
      const errors = character.validateForPlay();
      expect(errors).toEqual([]);
      
      // Add too many attribute increases
      character.setAttribute('agility', 'd12');
      character.setAttribute('smarts', 'd12');
      character.setAttribute('spirit', 'd12');
      character.setAttribute('strength', 'd12');
      character.setAttribute('vigor', 'd12');
      
      const errors2 = character.validateForPlay();
      expect(errors2.length).toBeGreaterThan(0);
    });
  });
});