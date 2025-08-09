import Skills from './Skills';
import Skill from './Skill';

describe('Skills', () => {
  describe('constructor', () => {
    it('should initialize with empty skills array by default', () => {
      const skills = new Skills();
      expect(skills.skills).toEqual([]);
    });

    it('should initialize with provided skills data', () => {
      const skillsData = {
        skills: [
          { name: 'Athletics', attribute: 'Agility', description: 'Physical fitness' },
          { name: 'Notice', attribute: 'Smarts', description: 'Awareness' }
        ]
      };
      const skills = new Skills(skillsData);
      expect(skills.skills).toHaveLength(2);
      expect(skills.skills[0].name).toBe('Athletics');
      expect(skills.skills[1].name).toBe('Notice');
    });
  });

  describe('getDefaultSkills', () => {
    it('should return an array of default skills', () => {
      const defaultSkills = Skills.getDefaultSkills();
      expect(Array.isArray(defaultSkills)).toBe(true);
      expect(defaultSkills.length).toBeGreaterThan(0);
    });

    it('should include core skills from Savage Worlds', () => {
      const defaultSkills = Skills.getDefaultSkills();
      const skillNames = defaultSkills.map(skill => skill.name);
      
      expect(skillNames).toContain('Athletics');
      expect(skillNames).toContain('Common Knowledge');
      expect(skillNames).toContain('Notice');
      expect(skillNames).toContain('Persuasion');
      expect(skillNames).toContain('Stealth');
    });

    it('should have correct attributes for default skills', () => {
      const defaultSkills = Skills.getDefaultSkills();
      const athletics = defaultSkills.find(s => s.name === 'Athletics');
      const commonKnowledge = defaultSkills.find(s => s.name === 'Common Knowledge');
      const notice = defaultSkills.find(s => s.name === 'Notice');
      const persuasion = defaultSkills.find(s => s.name === 'Persuasion');
      const stealth = defaultSkills.find(s => s.name === 'Stealth');
      
      expect(athletics.attribute).toBe('Agility');
      expect(commonKnowledge.attribute).toBe('Smarts');
      expect(notice.attribute).toBe('Smarts');
      expect(persuasion.attribute).toBe('Spirit');
      expect(stealth.attribute).toBe('Agility');
    });

    it('should have descriptions for all default skills', () => {
      const defaultSkills = Skills.getDefaultSkills();
      defaultSkills.forEach(skill => {
        expect(skill.description).toBeDefined();
        expect(skill.description.length).toBeGreaterThan(0);
      });
    });

    it('should return Skill instances', () => {
      const defaultSkills = Skills.getDefaultSkills();
      defaultSkills.forEach(skill => {
        expect(skill).toBeInstanceOf(Skill);
      });
    });
  });

  describe('addSkill', () => {
    it('should add a new skill to the skills array', () => {
      const skills = new Skills();
      const newSkill = new Skill({
        name: 'Fighting',
        attribute: 'Agility',
        description: 'Hand-to-hand combat'
      });
      
      skills.addSkill(newSkill);
      expect(skills.skills).toHaveLength(1);
      expect(skills.skills[0]).toBe(newSkill);
    });
  });

  describe('removeSkill', () => {
    it('should remove a skill at the specified index', () => {
      const skills = new Skills({
        skills: [
          new Skill({ name: 'Athletics', attribute: 'Agility' }),
          new Skill({ name: 'Notice', attribute: 'Smarts' }),
          new Skill({ name: 'Stealth', attribute: 'Agility' })
        ]
      });
      
      skills.removeSkill(1);
      expect(skills.skills).toHaveLength(2);
      expect(skills.skills[0].name).toBe('Athletics');
      expect(skills.skills[1].name).toBe('Stealth');
    });

    it('should handle removing skill at invalid index gracefully', () => {
      const skills = new Skills({
        skills: [
          new Skill({ name: 'Athletics', attribute: 'Agility' })
        ]
      });
      
      skills.removeSkill(-1);
      expect(skills.skills).toHaveLength(1);
      
      skills.removeSkill(5);
      expect(skills.skills).toHaveLength(1);
    });
  });

  describe('toJSON', () => {
    it('should return a JSON representation of the skills', () => {
      const skills = new Skills({
        skills: [
          new Skill({ name: 'Athletics', attribute: 'Agility', description: 'Physical fitness' }),
          new Skill({ name: 'Notice', attribute: 'Smarts', description: 'Awareness' })
        ]
      });
      
      const json = skills.toJSON();
      expect(json.skills).toHaveLength(2);
      expect(json.skills[0].name).toBe('Athletics');
      expect(json.skills[0].attribute).toBe('Agility');
      expect(json.skills[0].description).toBe('Physical fitness');
    });
  });
});