import Skill from './Skill';

export default class Skills {
  skills = []

  constructor(data = {}) {
    this.skills = data.skills || [];
  }

  addSkill(skill) {
    this.skills.push(skill);
  }

  removeSkill(index) {
    if (index >= 0 && index < this.skills.length) {
      this.skills.splice(index, 1);
    }
  }

  static getDefaultSkills() {
    const defaultSkills = [];

    // Athletics
    const athletics = new Skill({
      name: 'Athletics',
      attribute: 'Agility',
      description: 'Covers running, jumping, swimming, throwing, and catching'
    });
    defaultSkills.push(athletics);

    // Common Knowledge
    const commonKnowledge = new Skill({
      name: 'Common Knowledge',
      attribute: 'Smarts',
      description: 'General knowledge of the world'
    });
    defaultSkills.push(commonKnowledge);

    // Notice
    const notice = new Skill({
      name: 'Notice',
      attribute: 'Smarts',
      description: 'General awareness and perception'
    });
    defaultSkills.push(notice);

    // Persuasion
    const persuasion = new Skill({
      name: 'Persuasion',
      attribute: 'Spirit',
      description: 'Ability to convince others'
    });
    defaultSkills.push(persuasion);

    // Stealth
    const stealth = new Skill({
      name: 'Stealth',
      attribute: 'Agility',
      description: 'The ability to hide and move quietly'
    });
    defaultSkills.push(stealth);

    return defaultSkills;
  }

  toJSON() {
    return {
      skills: this.skills.map(s => s.toJSON ? s.toJSON() : s)
    };
  }
}